(ns cubes-test
  (:require
    [clojure.test :refer :all]
    [cubes]))

(deftest row->game
  (testing "A basic row can become a simple game"
    (is (= {1 [{:blue 1 :green 1} {:blue 1 :red 1}]} (cubes/row->game "Game 1: 1 blue, 1 green; 1 blue, 1 red")))
    (is (= {57 [{:blue 12 :red 97}]} (cubes/row->game "Game 57: 12 blue, 97 red")))
    (is (= {100 [{:green 1}]} (cubes/row->game "Game 100: 1 green")))))

(deftest can-read-and-reduce-file
  (testing "We can read the file and correctly represent it"
    (let [summary (cubes/file->summary "test_resources/cube/example_one.txt")]
      (is (= [{:blue 3 :red 4} {:red 1 :green 2 :blue 6} {:green 2}] (get summary 1)))
      (is (= [{:red 6 :blue 1 :green 3} {:blue 2 :red 1 :green 2}] (get summary 5))))))

(deftest can-determine-if-possible
  (testing "Possible with 12 red, 13 green and 14 blue cubes?"
    (is (= 8 (cubes/possible-game-sum "test_resources/cube/example_one.txt" {:red 12 :green 13 :blue 14})))
    (is (some? (cubes/possible-game-sum "test_resources/cube/full_input.txt" {:red 12 :green 13 :blue 14})))))

(deftest from-game-determine-minimum-possible
  (testing "For a single row, we can correctly determine the minimum constraints"
    (let [expected {1 {:red 4 :green 2 :blue 6}}]
      (is (= expected (cubes/minimum-possible {1 [{:blue 6} {:green 2} {:red 4}]})))
      (testing "The expected can be reduce to power"
        (is (= 48 (cubes/minimum-possible->power expected))))))
  (testing "From the example game, correctly works out the minimum constraints to make it possible"
    (let [expected {1 {:red 4 :green 2 :blue 6}
                    2 {:red 1 :green 3 :blue 4}
                    3 {:red 20 :green 13 :blue 6}
                    4 {:red 14 :green 3 :blue 15}
                    5 {:red 6 :green 3 :blue 2}}]
      (is (= expected (cubes/minimum-possible (cubes/file->summary "test_resources/cube/example_one.txt"))))
      (testing "The expected can be reduce to power"
        (is (= 2286 (cubes/minimum-possible->power expected)))))))
