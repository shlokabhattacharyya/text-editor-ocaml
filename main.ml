(* phase 1: raw mode, key decoding, clean exit *)

(* ----- terminal ----- *)

let orig_termios : Unix.terminal_io option ref = ref None

let disable_raw_mode () =
  match !orig_termios with
  | Some t -> Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH t
  | None -> ()

let enable_raw_mode () =
  let t = Unix.tcgetattr Unix.stdin in
  orig_termios := Some t;
  (* restore the terminal however we exit, including uncaught exceptions. *)
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

(* blocking read of one byte. retries if a signal interrupts the read
   (this matters in Phase 2 when we handle window resize). *)
let rec read_byte () : char =
  match Unix.read Unix.stdin buf 0 1 with
  | 0 -> exit 0 (* EOF *)
  | _ -> Bytes.get buf 0
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> read_byte ()

(* read one byte, giving up after [timeout] seconds. used to tell a
   lone Escape keypress apart from the start of an escape sequence. *)
let read_byte_timeout timeout : char option =
  match Unix.select [ Unix.stdin ] [] [] timeout with
  | [], _, _ -> None
  | _ -> Some (read_byte ())
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> None

(* arrow keys etc. arrive as multi-byte sequences:
     ESC [ A        Up            ESC [ 3 ~     Delete
     ESC [ H        Home          ESC [ 5 ~     Page Up
     ESC O H        Home (some terminals)                     *)
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
      (* ctrl-A is byte 1, ctrl-B is 2, ... so add 96 to get the letter. *)
      Ctrl (Char.chr (Char.code c + 96))
  | c -> Char c

(* ----- phase 1 demo loop ----- *)

let string_of_key = function
  | Char c -> Printf.sprintf "Char %C (byte %d)" c (Char.code c)
  | Ctrl c -> Printf.sprintf "Ctrl-%c" (Char.uppercase_ascii c)
  | Enter -> "Enter"
  | Tab -> "Tab"
  | Backspace -> "Backspace"
  | Delete -> "Delete"
  | Escape -> "Escape"
  | Up -> "Up"
  | Down -> "Down"
  | Left -> "Left"
  | Right -> "Right"
  | Home -> "Home"
  | End -> "End"
  | Page_up -> "Page_up"
  | Page_down -> "Page_down"

(* output post-processing is off, so we must send "\r\n" ourselves *)
let print_line s =
  print_string s;
  print_string "\r\n";
  flush stdout

let () =
  enable_raw_mode ();
  print_line "press keys to see how they decode. ctrl-Q to quit";
  let rec loop () =
    match read_key () with
    | Ctrl 'q' -> ()
    | k ->
        print_line (string_of_key k);
        loop ()
  in
  loop ()