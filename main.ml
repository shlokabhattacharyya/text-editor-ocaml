(* phase 1: raw mode, key decoding, clean exit 
 * phase 2: drawing the screen and moving the cursor *)

(* ----- terminal ----- *)

let orig_termios : Unix.terminal_io option ref = ref None

let disable_raw_mode () =
  match !orig_termios with
  | Some t -> Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH t
  | None -> ()

let enable_raw_mode () =
  let t = Unix.tcgetattr Unix.stdin in
  orig_termios := Some t;
  (* restore the terminal however we exit, including uncaught exceptions *)
  at_exit disable_raw_mode;
  let raw =
    { t with
      Unix.c_echo = false;   (* don't print typed keys *)
      c_icanon = false;      (* byte-at-a-time instead of line-at-a-time *)
      c_isig = false;        (* Ctrl-C / Ctrl-Z arrive as plain bytes *)
      c_ixon = false;        (* Ctrl-S / Ctrl-Q arrive as plain bytes *)
      c_icrnl = false;       (* Enter is '\r' (13), distinct from Ctrl-J *)
      c_opost = false;       (* no "\n" -> "\r\n" translation on output *)
      c_brkint = false;
      c_inpck = false;
      c_istrip = false;
      c_csize = 8;
      c_vmin = 1;            (* read blocks until at least 1 byte *)
      c_vtime = 0;
    }
  in
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH raw

(* ----- input ----- *)

type key =
  | Char of char        (* printable byte *)
  | Ctrl of char        (* Ctrl 'q', Ctrl 's', ... *)
  | Enter
  | Tab
  | Backspace
  | Delete
  | Escape
  | Up | Down | Left | Right
  | Home | End
  | Page_up | Page_down

let buf = Bytes.create 1

(* blocking read of one byte - retries if a signal interrupts the read *)
let rec read_byte () : char =
  match Unix.read Unix.stdin buf 0 1 with
  | 0 -> exit 0 (* EOF *)
  | _ -> Bytes.get buf 0
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> read_byte ()

(* read one byte, giving up after [timeout] seconds - used to tell a
   lone Escape keypress apart from the start of an escape sequence *)
let read_byte_timeout timeout : char option =
  match Unix.select [ Unix.stdin ] [] [] timeout with
  | [], _, _ -> None
  | _ -> Some (read_byte ())
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> None

(* arrow keys etc. arrive as multi-byte sequences:
     ESC [ A        Up            ESC [ 3 ~     Delete
     ESC [ H        Home          ESC [ 5 ~     Page Up
     ESC O H        Home (some terminals)                *)
let read_escape_sequence () : key =
  let next () = read_byte_timeout 0.05 in
  match next () with
  | None -> Escape
  | Some '[' -> (
      match next () with
      | Some 'A' -> Up
      | Some 'B' -> Down
      | Some 'C' -> Right
      | Some 'D' -> Left
      | Some 'H' -> Home
      | Some 'F' -> End
      | Some ('0' .. '9' as d) -> (
          match next () with
          | Some '~' -> (
              match d with
              | '1' | '7' -> Home
              | '4' | '8' -> End
              | '3' -> Delete
              | '5' -> Page_up
              | '6' -> Page_down
              | _ -> Escape)
          | _ -> Escape)
      | _ -> Escape)
  | Some 'O' -> (
      match next () with
      | Some 'H' -> Home
      | Some 'F' -> End
      | _ -> Escape)
  | Some _ -> Escape

let read_key () : key =
  match read_byte () with
  | '\x1b' -> read_escape_sequence ()
  | '\r' -> Enter
  | '\t' -> Tab
  | '\x7f' -> Backspace
  | c when Char.code c < 32 ->
      (* ctrl-A is byte 1, ctrl-B is 2, ... so add 96 to get the letter *)
      Ctrl (Char.chr (Char.code c + 96))
  | c -> Char c

(* ----- output ----- *)

let clear_screen = "\x1b[2J"   (* erase everything *)
let clear_line   = "\x1b[K"    (* erase from the cursor to the end of the line *)
let cursor_home  = "\x1b[H"    (* move the cursor to the top-left corner *)
let hide_cursor  = "\x1b[?25l"
let show_cursor  = "\x1b[?25h"

(* move the cursor to a row and column. the terminal counts from 1,
 * but our code counts from 0, hence the +1 *)
let cursor_to ~row ~col = Printf.sprintf "\x1b[%d;%dH" (row + 1) (col + 1)

(* send a string to the terminal right now *)
let write s =
  print_string s;
  flush stdout

(* ----- window size -----
 * OCaml's Unix module cannot natively tell us the size of the window, 
 * so we must ask the terminal itself:
 *   1. push the cursor 999 right and 999 down; it stops at the corner
 *   2. send ESC [ 6 n, which means "report where the cursor is"
 *   3. the terminal answers on stdin with  ESC [ rows ; cols R *)
let get_window_size () : int * int =
  write "\x1b[999C\x1b[999B\x1b[6n";
  let reply = Buffer.create 16 in
  let rec collect () =
    match read_byte_timeout 1.0 with
    | Some 'R' | None -> ()
    | Some c ->
        Buffer.add_char reply c;
        collect ()
  in
  collect ();
  try Scanf.sscanf (Buffer.contents reply) "\x1b[%d;%d" (fun r c -> (r, c))
  with Scanf.Scan_failure _ | Failure _ | End_of_file -> (24, 80)

(* ----- editor state -----
 * everything the editor knows - it is immutable: moving the cursor
 * produces a new record rather than changing this one *)
type editor = {
  rows : int;  (* window height *)
  cols : int;  (* window width *)
  cx : int;    (* cursor column, 0 = left edge *)
  cy : int;    (* cursor row, 0 = top *)
}

(* ----- drawing ----- *)

let welcome = "OCaml editor -- phase 2 -- Ctrl-Q to quit"

(* the text for one screen row - there is no file yet, so every row is a "~"
 * like vim's empty lines, with a welcome message a third of the way down *)
let row_text e y =
  if y = e.rows / 3 && String.length welcome < e.cols then
    let padding = (e.cols - String.length welcome) / 2 in
    "~" ^ String.make (max 0 (padding - 1)) ' ' ^ welcome
  else "~"

(* redraw the whole screen - the frame is assembled in a buffer and sent
 * with a single write, so the terminal never shows a half-drawn screen *)
let refresh_screen e =
  let b = Buffer.create 4096 in
  Buffer.add_string b hide_cursor;
  Buffer.add_string b cursor_home;
  for y = 0 to e.rows - 1 do
    Buffer.add_string b (row_text e y);
    Buffer.add_string b clear_line;
    (* no newline after the last row, or the screen would scroll *)
    if y < e.rows - 1 then Buffer.add_string b "\r\n"
  done;
  Buffer.add_string b (cursor_to ~row:e.cy ~col:e.cx);
  Buffer.add_string b show_cursor;
  write (Buffer.contents b)

(* ------ reacting to keys ----- *)

(* given the current state and a key, return the next state
 * [min and max keep the cursor inside the window] *)
let move e key =
  match key with
  | Left -> { e with cx = max 0 (e.cx - 1) }
  | Right -> { e with cx = min (e.cols - 1) (e.cx + 1) }
  | Up -> { e with cy = max 0 (e.cy - 1) }
  | Down -> { e with cy = min (e.rows - 1) (e.cy + 1) }
  | Home -> { e with cx = 0 }
  | End -> { e with cx = e.cols - 1 }
  | Page_up -> { e with cy = 0 }
  | Page_down -> { e with cy = e.rows - 1 }
  | _ -> e

(* ----- main loop ----- *)

let () =
  enable_raw_mode ();
  let rows, cols = get_window_size () in
  (* draw, wait for a key, compute the new state, repeat *)
  let rec loop e =
    refresh_screen e;
    match read_key () with
    | Ctrl 'q' -> write (clear_screen ^ cursor_home)
    | k -> loop (move e k)
  in
  loop { rows; cols; cx = 0; cy = 0 }