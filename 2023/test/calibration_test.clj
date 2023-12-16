(ns calibration-test
  (:require
    [clojure.test :refer :all]
    [calibration]
    [shared]))

(deftest row-to-digits
  (testing "A string of just two digits returns the first and last as digits"
    (is (= 12 (calibration/row->digits "12"))))
  (testing "Characters are ignored"
    (is (nil? (calibration/row->digits "abc"))))
  (testing "A row with only one digit doubles it"
    (is (= 77 (calibration/row->digits "7"))))
  (testing "Some more obscure examples"
    (is (= 87 (calibration/row->digits "eightfivesssxxmgthreethreeone1sevenhnz")))
    (is (= 11 (calibration/row->digits "one")))
    (is (= 55 (calibration/row->digits "pxvmbjprllmbfpzjxsvhc5")))
    (is (= 58 (calibration/row->digits "five3eightsfvftdxl")))
    (is (= 23 (calibration/row->digits "lmfkvgfzfmhxqrcvsgt28ssmhm5fivethree")))
    (is (= 95 (calibration/row->digits "nine15nine1three5")))
    (is (= 18 (calibration/row->digits "oneight")))
    (is (= 29 (calibration/row->digits "two1nine")))
    (is (= 83 (calibration/row->digits "eightwothree")))
    (is (= 13 (calibration/row->digits "abcone2threexyz")))))

; TODO (Yeah can we drop the duplicated letter?
#_(deftest word->digit
  (testing "can turn a row with words and digits into the correct form"
    (is (= "219" (calibration/words->digits calibration/numbers "two1nine")))
    (is (= "823" (calibration/words->digits calibration/numbers "eighttwothree")))
    (is (= "abc123xyz" (calibration/words->digits calibration/numbers "abcone2threexyz"))))
  (testing "Overlap results in both numbers being returned, with order being maintained"
    (is (= "18t" (calibration/words->digits calibration/numbers "oneight")))
    (is (= "21" (calibration/words->digits calibration/numbers "twone")))
    (is (= "x2134" (calibration/words->digits calibration/numbers "xtwone3four"))))
  (testing "A single word is returned as just the digit"
    (is (= "1" (calibration/words->digits calibration/numbers "one")))))

(deftest file-to-rows
  (testing "Can read the file into rows"
    (is (= ["12abc123" "1a2b3c"]
           (shared/file->rows "test_resources/calibration/test_small.txt")))))

(deftest input-example
  (testing "Reading the defined input is correct"
    (is (= 142 (calibration/file->calibration-score "test_resources/calibration/test_example_from_aoc.txt")))))

(deftest input-with-spelled-digits
  (testing "Reading the defined input is correct"
    (is (= 281 (calibration/file->calibration-score "test_resources/calibration/test_second_example_from_aoc.txt")))))
