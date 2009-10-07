open Database;;
(* Test Code *)
let d = Database(
  (
    "0",
    Table(
      "person",
      (Field("name",StringField(200))::[])
    )
  ) ::
  (
    "1",
    Table(
      "address",
      (Field("line1",StringField(200))::[])
    )
  ) ::
  [],
  Rel( ("0", "addressOf") , ("1","homeAddress"), Many_to_one) ::[]
)
let a = new data "main" d

