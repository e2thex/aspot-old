open Printf;;

exception MissingArgument of string;;
exception MissingAction;;
exception MissingClassFieldDef of string*string;;
(*** START DATABASE DEF ***)

(* Defining the structures for a table (the mose base of classes, it only has
fields not other objects *)

type table = Table of string * field list (* Table(name, list of fields) *)
and  field = Field of string * field_type (* Field(name, type) *)
and  field_type = 
       | IntField 
       | BoolField 
       | StringField of int


(* Defining the relastionships between tables so that we can create comlicated
objects *)

type relationship = Rel of (string*string) * (string*string) * rel_type 
(* Rel( (table1_id, name of table1), (table2_id, name of table2), type *)
and  rel_type =
          | Many_to_many
          | Many_to_one
          | One_to_one 

(* Defining dtabases which have a list of tables and reloationships that can be
melded togeather *)

type database = Database of ( (string * table ) list * relationship list )

(* Defining class which will desend from tables, but also have references to
other classes *)

type oclass = Class of string * element list (* Class(name, list of elements) *)
and  element = Element of string * element_type(* Element(name, type) *)
and  element_type = 
            | RelList
            | RelItem
            | IntElement
            | BoolElement
            | StringElement of int


exception NoClass of string;;
exception NoField of string;;
(*** END DATABASE DEF ***)
(*** START QUERY DEF ***)
type query = { 
  database    : string    ; 
  root        : query_item;
}
(* a query_item is a node of the object map *)
and query_item = 
  | Root
  | ClassOf of string 
  | ChildOf of query_item * query_item
  | LimitBy of query_item * bool_item
(* limit items are clause that limit a node*)
and limit_item = QueryItem of query_item | BoolItem of bool_item 
(* bool_items are parts of clause that can be tested for truth*)
and bool_item =
  | Compare of compare_item * compare_operator * compare_item
  | And     of bool_item * bool_item
  | OR      of bool_item * bool_item
(*compare items are the the base of bool_items and are the actural compare*)
and compare_item = QueryItem of query_item | Primative of primative_item
(*compare_operators are use to describe the type of compare*)
and compare_operator = Eq | Ne | GT | LT | GE | LE 
(*primative_items are used for compares *)
and primative_item = String of string | Int of int | Float of float
(*** END QUERY DEF ***)

(* Building the class data which house the definitions for a dataset *)
class data name_in database_in =
  let Database(tables_in, rels_in) = database_in in 

  (* FInds the table uid by the name of the table
  @param
    name_in: the name of the table for which one is looking
    table_list_in : a list of tuplets (uid, Table)
  RETURN
    the uid from the tuplet where the Table name = name_in
  *)
  let rec find_table_uid_by_name name_in table_list_in =
     match table_list_in with
       | [] -> raise (NoClass name_in)
       | ((uid:string), Table(n,fl)) :: tail -> 
         if n = name_in then uid
         else find_table_uid_by_name name_in tail
  in

  (* FInds the Table by the uid of the list
  @param
    uid_in: the uid of the table for which one is looking
    table_list_in : a list of tuplets (uid, Table)
  RETURN
    the Table from the tuplet where the uid = uid_in
  *)
  let rec find_table_by_uid uid_in table_list =
     match table_list with
       | [] -> raise (NoClass uid_in)
       | ((uid:string), Table(n,fl)) :: tail -> 
         if uid = uid_in then Table(n,fl)
         else find_table_by_uid uid_in tail
  in
  (* FInds the Field by the name of the field
  @param
    name_in: the name of the Field for which one is looking
    filed_list_in : a list of Field 
  RETURN
    the Field where the Field name  = name_in
  *)
  let rec find_field name_in field_list_in =
     match field_list_in with
       | [] -> raise (NoField name_in)
       | Field(n,t) :: tail -> 
         if n = name_in then Field(n,t) 
         else find_field name_in tail
  in
  (* Translate a field into an element
  @param
    name_in: the name of the Field for which one is looking
    filed_list_in : a list of Fields 
  RETURN
    an Element of the type corisponding to the field type 
    where the Field name  = name_in
  *)
  let get_element_from_field name_in field_list_in =
    let Field(n,t) = find_field name_in field_list_in in 
    match t with
      | IntField -> Element(n, IntElement)
      | BoolField -> Element(n, BoolElement)
      | StringField(i) -> Element(n, StringElement(i))
  in
  (* build object *)
  object (database)
    val name    : string                     = name_in
    val tables : (string*table) list         = tables_in
    val rels    : (relationship) list = rels_in
    method private get_table_uid_by_name name_in = 
      find_table_uid_by_name name_in tables
    method private get_table_by_uid uid_in =
      find_table_by_uid uid_in tables
    method private get_element name_in uid_in = 
      let Table(n,fl) = database#get_table_by_uid uid_in in
      
      (try get_element_from_field name_in fl
        with NoField(t) -> Element("todd" , RelItem)
        )



    method obj_class name_in =
      object (obj_class)
        val uid = database#get_table_uid_by_name name_in
        method uid  = uid
        method name = 
          let Table(n,fl) = database#get_table_by_uid uid in n
        method element name_in =
          let Element(n,t) = database#get_element (name_in) (obj_class#uid) 
          in
          object (element)
            val name = n
            val e_type = t
            method name = name
            method element_type = e_type
            (*method ref_class*)
          end
      end
  end
  ;;



(* String_clause finds a clause in the string that starts
 * with start_char and ends with end_char
 * For example stirng cluase "bob(job(sue))" '(' ')' would 
 * return "joe(sue)"
 * *)
let string_clause s start_char end_char = 
  let rec find_limit_clause s start loc open_count start_char end_char =
    if open_count = 0 then String.sub s start (loc - start - 1)
    else 
      let new_loc = 
        min (try String.index_from s (loc) start_char 
                with Not_found -> (String.length s)-1 
            ) 
            (try String.index_from s (loc) end_char   
                with Not_found -> (String.length s)-1 
            )
      in
      let new_open_count = 
        if s.[new_loc] = end_char then open_count-1
        else if s.[new_loc] = start_char then open_count +1 
        else raise(Not_found)
      in
      find_limit_clause s start (new_loc + 1) new_open_count start_char end_char
  and start = 
    String.index s start_char
  in 
  find_limit_clause s (start + 1) (start +1)  1 start_char end_char
;;

(* function to take a xquery string and convert it to a query_item *)
let rec querify xquery_s = 
  let rec limitify xquery_s =
    Compare( Primative(String(xquery_s)), Eq, Primative(String("true")))
  in 
  (* find the next break *)
  let index =  
        min ( try String.index xquery_s '/'
                with Not_found -> (String.length xquery_s)-1 
            ) 
            (try String.index  xquery_s '['   
                with Not_found -> (String.length xquery_s)-1 
            )
  in
  (* find the type of break*)
  match xquery_s.[index] with
    | '/' -> 
      (*split query into parent and child*)
      let parent_s = String.sub xquery_s 0 index
      and child_xquery_s =
        String.sub 
          xquery_s 
          (index +1)
          ( (String.length xquery_s) - 1 - index )
      in
      (*if no parent the the parent is root*)
      if index = 0 then 
        ChildOf( Root             , (querify child_xquery_s) ) 
      else
        ChildOf( ClassOf(parent_s), (querify child_xquery_s) )
    | '[' ->  (** need to check to insure there is a obj_s **)
      let base_s = String.sub xquery_s 0 index
      and limit_xquery_s = string_clause xquery_s '[' ']'
      in
      if index = 0 then 
        LimitBy( Root             , (limitify limit_xquery_s) ) 
      else
        LimitBy( ClassOf(base_s)  , (limitify limit_xquery_s) )
    | _   ->   ClassOf (xquery_s)
;;
(*
(*used for testing*)
let rec test_query_item q_item =
  match q_item with 
    | ClassOf(name)         -> name
    | ChildOf(par,child)    -> (test_query_item par)^"/"^(test_query_item child)
    | _ ->"nothing"

let update args =
  let xquery = 
  if (Hashtbl.mem args "xquery") then
    Hashtbl.find args "xquery"
  else
    raise (MissingArgument "xquery")
    in
  let value = 
  if (Hashtbl.mem args "value") then
    Hashtbl.find args "value"
  else
    raise (MissingArgument "value")
    in
  xquery^value
;;
(*used to return arguments*)
let get args =
  let xquery = 
  if (Hashtbl.mem args "xquery") then
    Hashtbl.find args "xquery"
  else
    raise (MissingArgument "xquery")
    in

    let q = querify xquery in
    test_query_item q
;;
let test_action args meth =
  
  let action = 
    if (Hashtbl.mem args "action") then
        Hashtbl.find args "action"
    else ""
  in
  let meth =
  match action with
    | "get"     -> `GET
    | "update"  -> `POST
    | _         -> meth
  in
  match meth with
    |`GET   -> get args
    |`POST  -> update args
    | _     -> raise (MissingAction)
  
;;
*)
0
