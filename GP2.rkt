#lang racket/base
(require redex)
(require racket/match)
(require racket/list)

; Definição da linguagem GP2 com base na sintaxe da Figura 1
(define-language GP2
  [Program ::= (MainDecl) (listof ProcedureDecl) (listof RuleDecl)]
  [MainDecl ::= ("Main" "->" CommandSeq)]
  [ProcedureDecl ::= (ProcedureID "->" "[" LocalDecl "]"
                      CommandSeq)]
  [LocalDecl ::= (RuleDecl) (ProcedureDecl)]
  [RuleDecl ::= (RuleID "=>" CommandSeq)] ; Definir conforme a representação de regras
  [CommandSeq ::= (listof Command)]
  [Command ::= (Block) ("if" Block "then" Block "else" Block)
               ("try" Block "then" Block "else" Block)
               (SimpleCommand)]
  [Block ::= ("(" CommandSeq ")")]
  [SimpleCommand ::= (RuleSetCall) (ProcedureCall) "break"
                     "skip" "fail"]
  [RuleSetCall ::= (RuleID "{" (listof RuleID) "}")]
  [ProcedureCall ::= (ProcedureID)]
  [ProcedureID ::= variable]
  [RuleID ::= variable]
  
  ; Definição de termos adicionais para suportar as regras
  [G ::= (CommandSeq)] ; G representa um estado que é uma sequência de comandos
  [H ::= (CommandSeq)] ; H representa o estado resultante após a execução de G
  [P ::= (Command)] ; P representa um comando individual
  [Plinha ::= (Command)] ; P' representa o comando resultante após a execução de P
  [C ::= (Condition)] ; C representa uma condição que pode ser verdadeira ou falsa
  [Condition ::= "true" "false"]
)

; Implementação das regras de redução conforme a Figura 2
(define r
  (reduction-relation GP2
    #:domain Command
    ; Regra call1
    (--> (call1 R G) H "call1")
    
    ; Regra call2
    (--> (call2 R G) "fail" "call2")
    
    ; Regra seq1
    (--> (seq1 P G) (seq Plinha H) "seq1")
    
    ; Regra seq2
    (--> (seq2 P G) (seq P Q H) "seq2")
    
    ; Regra seq3
    (--> (seq3 P G) "fail" "seq3")
    
    ; Regra if1
    (--> (if1 C G) H "if1")
    
    ; Regra if2
    (--> (if2 C G) "fail" "if2")
    
    ; Regra try1
    (--> (try1 C G) H "try1")
    
    ; Regra try2
    (--> (try2 C G) (try C P Q G) "try2")
    
    ; Regra alap1
    (--> (alap1 P G) H "alap1")
    
    ; Regra alap2
    (--> (alap2 P G) G "alap2")
    
    ; Regra alap3
    (--> (alap3 P G) (seq Plinha (break H)) "alap3")
    
    ; Regra break
    (--> (break1 (break P G)) (break G) "break1")
  )
)

; Função para aplicar a relação de redução a um termo
(define (apply-reduction term)
  (redex-match GP2 Command term)
  (let* ((result (apply-reduction-relation r term))
         (reduced? (first result))
         (new-term (second result)))
    (if reduced?
        (printf "Termo reduzido: ~a\n" new-term)
        (printf "Redução não aplicável.\n"))))


; Testes de exemplo
(apply-reduction '(if ("(" skip ")") then ("(" skip ")") else ("(" skip ")")))

