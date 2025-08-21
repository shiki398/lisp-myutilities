(defun assert-report (ok expected actual form &optional caller)
  "assert (result:結果(expectedとactualの比較) expected:期待値 actual:実際の値 form:計算式 caller:呼び出し元)
  resultがnilのとき、失敗のメッセージを出力。"
  (when (not ok)
        (format t "~a 期待値：~a~\t実績値：~a~\t評価式：~a~%" (if caller (concatenate 'string "【" caller "】") "") expected actual form))
  ok)

(defmacro assert-base (fn compare expected &optional caller)
  "fn:計算式 compware:比較用の式 expected：期待値 caller:呼び出し元
  使い方： 
    (assert-base (+ 1) #'equal 1)
       → (+ 1)の値が1とequalであることを期待。
    (my-assert 3 1 #'(lambda (expected actual) (= expected (+ actual 2))))
      → 3と(+ 1 2)が=であることを期待。"
  (let ((ok (gensym)))
    `(let ((,ok ,fn))
        (assert-report (funcall ,compare ,expected ,ok)
                        ,expected
                        ,ok
                        ',fn
                        ,caller))))

(defmacro assert-equal (expected fn &optional (compare #'equal) caller)
  "計算式(fn)の結果が、期待値(expected)通りであることを確認する。
  expected:期待値 fn:計算式 compare:比較用の関数(default:equal) caller:呼び出し元
  使い方：
    (assert-equal 1 (+ 1))        → T
    (assert-equal 1 (+ 1 2))      → NIL
    (assert-equal 1 1.0)          → NIL
    (assert-equal 1 1.0 #'equalp) → T"
  `(assert-base ,fn ,compare ,expected ,caller))
(defmacro assert-t (fn)
  "計算式(fn)の結果が、nilではないことを確認する。
  fn:計算式
  使い方：
    (assert-t (+ 1))              → T
    (assert-t ())                 → NIL"
  `(assert-base ,fn
     (lambda (expected ok)
       (progn expected) ;明示的な無視
       (not (null ok)))
     t))
(defmacro assert-nil (fn)
  "計算式(fn)の結果が、nilであることを確認する。
  fn:計算式
  使い方：
    (assert-nil (+ 1))              → NIL
    (assert-nil ())                 → T"
  `(assert-base ,fn
     (lambda (expected ok)
       (progn expected) ;明示的な無視
       (null ok))
     nil))

(defmacro deftest (test-name &body body)
  "テストを定義する。
    test-name:テスト名
    body: テスト
  使い方：
    (deftest test-test
      (assert-t 1)
      (assert-nil nil)
      (assert-equal 1 1))
    → (test-test)
    　→ テスト開始( TEST-TEST )****
        テスト終了( TEST-TEST )****
        結果：成功:3 失敗:0
        **************************
        T
      ※ 1つでもエラーがあればNILを返却。"
  (let ((test (gensym)) (success-count (gensym)) (fail-count (gensym)))
    `(defun ,test-name ()
       (format t "テスト開始( ~a )****~%" ',test-name)
       (let ((,success-count 0) (,fail-count 0))
         (dolist (test (list ,@body))
           (if test (incf ,success-count) (incf ,fail-count)))
         (format t "テスト終了( ~a )****~%結果：成功:~a 失敗:~a~%**************************" ',test-name ,success-count ,fail-count)
         (= 0 ,fail-count)))))
