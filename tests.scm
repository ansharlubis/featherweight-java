(module tests mzscheme

  (provide tests-for-check)

  (define tests-for-check
    '(
      (test-c "
class a extends object {
  field object f1
  constructor (f1: object) {
    super()
    set access this f1 = f1
  }
}
class b extends object {
  constructor () { super() }
}
class c extends a {
  field object f2
  constructor (f1: object, f2: object) {
    super(f1)
    set access this f2 = f2
  }
}
new c(new b(), new b())" c)

      (test-pair "
class a extends object {
  constructor () { super() }
}
class b extends object {
  constructor () { super() }
}
class pair extends object {
  field object fst
  field object snd
  constructor (fst: object, snd: object) {
    super()
    set access this fst = fst
    set access this snd = snd
  }
  method pair setfst (newfst: object) {
    new pair(newfst, access this snd)
  }
}
send new pair(new a(), new b()) setfst(new b())
" pair)


      ))


  )