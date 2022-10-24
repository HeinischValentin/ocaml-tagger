let db_dir = "/home/valle/.taggerdb"

let save_entry c t =
    let oc = open_out_gen [Open_append; Open_creat] 0o666 db_dir
    in
    Printf.fprintf oc "%s %s\n" c t;
    close_out oc;
    Printf.printf "saved entry.\n"

let print_entries l =
    if l != [] then
        begin
            Printf.printf "Content\tTags\n";
            Printf.printf "-------------\n";
            let print_line line =
                let line_sp = String.split_on_char ' ' line
                in
                Printf.printf "%s\t%s\n" (List.nth line_sp 0) (List.nth line_sp 1);
            in
            List.iter print_line l;
            Printf.printf "\n"
        end
    else
        Printf.printf "No entries to print.\n"

let filter_entries t =
    try
        let ic = open_in db_dir
        in
        let rec build_list l =
            let check_line =
                try
                    let line = input_line ic in
                    if List.mem t (String.split_on_char ',' (List.nth (String.split_on_char ' ' line) 1)) || t ="*" then
                        line
                    else
                        "!NO_FILTER_MATCH"
                with End_of_file ->
                    close_in_noerr ic;
                    ""
            in
            match check_line with
            | ""                 -> l
            | "!NO_FILTER_MATCH" -> build_list l
            | cont               -> build_list (List.append l [cont])
        in
        build_list []
    with _ ->
        Printf.printf "Failed opening file\n";
        []

let arg_parse = function
    | [|_; "-a"; c; t|] -> save_entry c t
    | [|_; "-q"; q|]    -> print_entries (filter_entries q)
    | [|_; "-p"|]       -> print_entries (filter_entries "*")
    | _                 -> Printf.printf "Invalid arguments."

let _ =
    if Array.length Sys.argv > 1 then
        arg_parse Sys.argv
    else
        Printf.printf "Tag Database v1.0
Usage:
    - tagdb -a <CONTENT> <TAG1,TAG2,..>     Adds listing with content and tag(s).
    - tagdb -q <FILTER>                     Query for listings with tag.
    - tagdb -p                              Prints out all listings.\n"

