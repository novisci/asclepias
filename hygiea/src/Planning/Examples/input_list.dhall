-- TODO write a converter if needed so that programmer can just provide record type in dhall
-- but dhallFromCsv gets passed this type
let input = List { concepts : Text, facts : Text, begin : Integer, end : Integer }

in input
