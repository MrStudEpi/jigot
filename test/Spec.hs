--
-- EPITECH PROJECT, 2022
-- glados
-- File description:
-- Spec
--

import Test.HUnit

import ParserTest
import ASTest
import ASMTest
import CoreTest

main :: IO Counts
-- unstable testscore
main = runTestTT $ TestList [ testsCore, testsParser, testsASTS, testsASM ]
