(module tests mzscheme

  (provide tests-for-check)

  (define tests-for-check
    '(
      (test-a "
class a extends object {
 initialize () {
  super()
 }
}
class b extends object {
 initialize () {
  super()
 }
}
class pair extends object {
 field object fst
 field object snd
 method void initialize (fst_arg: object, snd_arg: object) {
  begin
   super();
   set fst = fst_arg;
   set snd = snd_arg
  end
 }
 method pair setfst (newfst: object) {
  new pair (newfst, snd)
 }
}
send new pair (new a (), new b()) setfst(new b())
" pair)
      ))


  )