val pari: PartialFunction[Int, String] = {
  case x if x % 2 == 0 => x + " è pari"
}

val dispari: PartialFunction[Int, String] = {
  case x if x % 2 == 1 => x + " è dispari"
}

val sample = 1 to 10

val listaNumeri = sample map (pari orElse dispari)

val listaNumeriPari = sample collect pari

val listaNumeriDispari = sample collect dispari
