module Flow.Parser.ModuleSpec (spec) where

import "hspec" Test.Hspec (Spec, describe, it)
import "text" Data.Text qualified as Text

import Flow.Parser.Helpers (testParser)
import Flow.Parser.Module qualified as PMod

spec :: Spec
spec = describe "Module parser (minimal subset)" do
  it "parses mod declaration 'mod m;'" do
    testParser "mod m;" PMod.pModule Nothing

  it "parses empty mod definition 'mod m { }'" do
    testParser "mod m { }" PMod.pModule Nothing

  it "parses use leaf 'use std::io;'" do
    testParser "use std::io;" PMod.pModule Nothing

  it "parses use leaf-as 'use std::io as IO;'" do
    testParser "use std::io as IO;" PMod.pModule Nothing

  it "parses nested use 'use std::{io, fs::{read, write}};'" do
    testParser "use std::{io, fs::{read, write}};" PMod.pModule Nothing

  it "parses minimal items: struct, enum, type alias, fn, let" do
    let src = Text.unlines
          [ "struct S {}"
          , "enum E { A, B }"
          , "type Pair<X, Y> = (X, Y)"
          , "fn add(a: i32, b: i32) -> i32 { }"
          , "let x: i32 = 42;"
          ]
    testParser src PMod.pModule Nothing


