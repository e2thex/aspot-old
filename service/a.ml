open Printf;;


class data type_in uri_in uuid_in =
  object (data)
    val itype : string = type_in
    val uri  : string = uri_in
    val uuid : string = uuid_in
    method itype = itype
    method uri = uri
    method uuid = uuid
  end
  ;;

type item = {uri : string ; uuid : string ; itype : string}
let newItem uri_in uuid_in type_in = 
  {uri=uri_in ; uuid=uuid_in ; itype=type_in};;
let joe = newItem "bob" "sue" "joe" in
print_string joe.uri

