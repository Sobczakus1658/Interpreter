{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module ParGramatyka
  ( happyError
  , myLexer
  , pProgram
  ) where

import Prelude

import qualified AbsGramatyka
import LexGramatyka
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap4 = HappyWrap4 ((AbsGramatyka.BNFC'Position, AbsGramatyka.Ident))
happyIn4 :: ((AbsGramatyka.BNFC'Position, AbsGramatyka.Ident)) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap4 x)
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> HappyWrap4
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
newtype HappyWrap5 = HappyWrap5 ((AbsGramatyka.BNFC'Position, Integer))
happyIn5 :: ((AbsGramatyka.BNFC'Position, Integer)) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap5 x)
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> HappyWrap5
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
newtype HappyWrap6 = HappyWrap6 ((AbsGramatyka.BNFC'Position, String))
happyIn6 :: ((AbsGramatyka.BNFC'Position, String)) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap6 x)
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> HappyWrap6
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
newtype HappyWrap7 = HappyWrap7 ((AbsGramatyka.BNFC'Position, AbsGramatyka.Program))
happyIn7 :: ((AbsGramatyka.BNFC'Position, AbsGramatyka.Program)) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap7 x)
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> HappyWrap7
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
newtype HappyWrap8 = HappyWrap8 ((AbsGramatyka.BNFC'Position, AbsGramatyka.FnDef))
happyIn8 :: ((AbsGramatyka.BNFC'Position, AbsGramatyka.FnDef)) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap8 x)
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> HappyWrap8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
newtype HappyWrap9 = HappyWrap9 ((AbsGramatyka.BNFC'Position, AbsGramatyka.Arg))
happyIn9 :: ((AbsGramatyka.BNFC'Position, AbsGramatyka.Arg)) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap9 x)
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> HappyWrap9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
newtype HappyWrap10 = HappyWrap10 ((AbsGramatyka.BNFC'Position, [AbsGramatyka.Arg]))
happyIn10 :: ((AbsGramatyka.BNFC'Position, [AbsGramatyka.Arg])) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap10 x)
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> HappyWrap10
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
newtype HappyWrap11 = HappyWrap11 ((AbsGramatyka.BNFC'Position, AbsGramatyka.ArgType))
happyIn11 :: ((AbsGramatyka.BNFC'Position, AbsGramatyka.ArgType)) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap11 x)
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> HappyWrap11
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
newtype HappyWrap12 = HappyWrap12 ((AbsGramatyka.BNFC'Position, [AbsGramatyka.ArgType]))
happyIn12 :: ((AbsGramatyka.BNFC'Position, [AbsGramatyka.ArgType])) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap12 x)
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> HappyWrap12
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
newtype HappyWrap13 = HappyWrap13 ((AbsGramatyka.BNFC'Position, AbsGramatyka.Block))
happyIn13 :: ((AbsGramatyka.BNFC'Position, AbsGramatyka.Block)) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap13 x)
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> HappyWrap13
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
newtype HappyWrap14 = HappyWrap14 ((AbsGramatyka.BNFC'Position, [AbsGramatyka.Stmt]))
happyIn14 :: ((AbsGramatyka.BNFC'Position, [AbsGramatyka.Stmt])) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap14 x)
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> HappyWrap14
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
newtype HappyWrap15 = HappyWrap15 ((AbsGramatyka.BNFC'Position, AbsGramatyka.Stmt))
happyIn15 :: ((AbsGramatyka.BNFC'Position, AbsGramatyka.Stmt)) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap15 x)
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> HappyWrap15
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
newtype HappyWrap16 = HappyWrap16 ((AbsGramatyka.BNFC'Position, AbsGramatyka.Item))
happyIn16 :: ((AbsGramatyka.BNFC'Position, AbsGramatyka.Item)) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap16 x)
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> HappyWrap16
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
newtype HappyWrap17 = HappyWrap17 ((AbsGramatyka.BNFC'Position, [AbsGramatyka.Item]))
happyIn17 :: ((AbsGramatyka.BNFC'Position, [AbsGramatyka.Item])) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap17 x)
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> HappyWrap17
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
newtype HappyWrap18 = HappyWrap18 ((AbsGramatyka.BNFC'Position, AbsGramatyka.Type))
happyIn18 :: ((AbsGramatyka.BNFC'Position, AbsGramatyka.Type)) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap18 x)
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> HappyWrap18
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
newtype HappyWrap19 = HappyWrap19 ((AbsGramatyka.BNFC'Position, [AbsGramatyka.Type]))
happyIn19 :: ((AbsGramatyka.BNFC'Position, [AbsGramatyka.Type])) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap19 x)
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> HappyWrap19
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
newtype HappyWrap20 = HappyWrap20 ((AbsGramatyka.BNFC'Position, AbsGramatyka.Expr))
happyIn20 :: ((AbsGramatyka.BNFC'Position, AbsGramatyka.Expr)) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap20 x)
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> HappyWrap20
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
newtype HappyWrap21 = HappyWrap21 ((AbsGramatyka.BNFC'Position, AbsGramatyka.Expr))
happyIn21 :: ((AbsGramatyka.BNFC'Position, AbsGramatyka.Expr)) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap21 x)
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> HappyWrap21
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
newtype HappyWrap22 = HappyWrap22 ((AbsGramatyka.BNFC'Position, AbsGramatyka.Expr))
happyIn22 :: ((AbsGramatyka.BNFC'Position, AbsGramatyka.Expr)) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap22 x)
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> HappyWrap22
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
newtype HappyWrap23 = HappyWrap23 ((AbsGramatyka.BNFC'Position, AbsGramatyka.Expr))
happyIn23 :: ((AbsGramatyka.BNFC'Position, AbsGramatyka.Expr)) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap23 x)
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> HappyWrap23
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
newtype HappyWrap24 = HappyWrap24 ((AbsGramatyka.BNFC'Position, AbsGramatyka.Expr))
happyIn24 :: ((AbsGramatyka.BNFC'Position, AbsGramatyka.Expr)) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap24 x)
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> HappyWrap24
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
newtype HappyWrap25 = HappyWrap25 ((AbsGramatyka.BNFC'Position, AbsGramatyka.Expr))
happyIn25 :: ((AbsGramatyka.BNFC'Position, AbsGramatyka.Expr)) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap25 x)
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> HappyWrap25
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
newtype HappyWrap26 = HappyWrap26 ((AbsGramatyka.BNFC'Position, AbsGramatyka.Expr))
happyIn26 :: ((AbsGramatyka.BNFC'Position, AbsGramatyka.Expr)) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap26 x)
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> HappyWrap26
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
newtype HappyWrap27 = HappyWrap27 ((AbsGramatyka.BNFC'Position, [AbsGramatyka.Expr]))
happyIn27 :: ((AbsGramatyka.BNFC'Position, [AbsGramatyka.Expr])) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap27 x)
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> HappyWrap27
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
newtype HappyWrap28 = HappyWrap28 ((AbsGramatyka.BNFC'Position, AbsGramatyka.AddOp))
happyIn28 :: ((AbsGramatyka.BNFC'Position, AbsGramatyka.AddOp)) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap28 x)
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> HappyWrap28
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
newtype HappyWrap29 = HappyWrap29 ((AbsGramatyka.BNFC'Position, AbsGramatyka.MulOp))
happyIn29 :: ((AbsGramatyka.BNFC'Position, AbsGramatyka.MulOp)) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap29 x)
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> HappyWrap29
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
newtype HappyWrap30 = HappyWrap30 ((AbsGramatyka.BNFC'Position, AbsGramatyka.RelOp))
happyIn30 :: ((AbsGramatyka.BNFC'Position, AbsGramatyka.RelOp)) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap30 x)
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> HappyWrap30
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x40\x08\x09\xe4\x7f\x0e\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x21\x24\x90\xff\x39\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x08\x01\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x80\x04\xb0\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x04\x00\x10\x04\x07\x00\x00\x00\xa4\x10\x40\xda\xe1\x00\x00\x00\x00\x10\x00\x40\x10\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x84\x90\x00\x92\xe0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x08\x21\x81\xfc\xcf\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x80\x10\x02\x40\x12\x1c\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x40\x08\x01\x20\x09\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x10\x52\x00\x00\x00\x00\x20\x84\x00\x90\x04\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x01\x80\x90\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x84\x10\x00\x92\xe0\x00\x00\x00\x80\x10\x02\x40\x10\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\x08\x00\x41\x70\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x10\x02\x40\x10\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x08\x01\x20\x08\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x04\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x08\x01\x20\x09\x0e\x00\x00\x00\x08\x21\x00\x24\xc1\x01\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x50\x00\x20\xa4\x00\x00\x00\x00\x40\x08\x01\x20\x09\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x84\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x20\xa4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x28\x00\x10\x52\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x40\x00\x20\xa4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x84\x04\xf2\x3f\x07\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x21\x05\x00\x00\x00\x00\x50\x00\x20\xa4\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x10\x52\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x08\x01\x20\x09\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram_internal","Ident","Integer","String","Program","FnDef","Arg","ListArg","ArgType","ListArgType","Block","ListStmt","Stmt","Item","ListItem","Type","ListType","Expr6","Expr5","Expr4","Expr3","Expr2","Expr1","Expr","ListExpr","AddOp","MulOp","RelOp","'!'","'!='","'%'","'&'","'&&'","'('","')'","'*'","'+'","','","'-'","'->'","'/'","';'","'<'","'<='","'='","'=='","'>'","'>='","'boolean'","'else'","'endif'","'false'","'if'","'int'","'lambda'","'return'","'string'","'true'","'void'","'while'","'{'","'||'","'}'","L_Ident","L_integ","L_quoted","%eof"]
        bit_start = st Prelude.* 69
        bit_end = (st Prelude.+ 1) Prelude.* 69
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..68]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x01\x00\xe9\xff\x00\x00\x28\x00\x00\x00\x00\x00\xeb\xff\x00\x00\x00\x00\x00\x00\x01\x00\xf3\xff\x00\x00\x00\x00\xfe\xff\x05\x00\x55\x00\x0a\x00\x26\x00\x0b\x00\x27\x00\x0b\x00\x00\x00\x00\x00\x00\x00\x34\x00\x00\x00\x38\x00\x48\x00\x00\x00\x00\x00\x00\x00\x3d\x00\x01\x00\x00\x00\x00\x00\x32\x00\x57\x00\x44\x00\x4d\x00\x57\x00\x00\x00\x70\x00\x57\x00\x00\x00\x52\x00\x4b\x00\x00\x00\x5a\x00\x1e\x00\x00\x00\x00\x00\x57\x00\x53\x00\x00\x00\x53\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x53\x00\x00\x00\x00\x00\x53\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x60\x00\x62\x00\x00\x00\x57\x00\x57\x00\x65\x00\x74\x00\x81\x00\x00\x00\x67\x00\x70\x00\x57\x00\x00\x00\xfe\xff\x00\x00\x05\x00\x00\x00\x00\x00\x7b\x00\x00\x00\x7d\x00\x7b\x00\x87\x00\x8d\x00\x98\x00\x86\x00\x1e\x00\x00\x00\x99\x00\x00\x00\x01\x00\x95\x00\x00\x00\x1e\x00\x7c\x00\x80\x00\x00\x00\x1e\x00\x00\x00\xa4\x00\xab\x00\x00\x00\x00\x00\x57\x00\x00\x00\x00\x00\x9f\x00\x00\x00\x31\x00\x00\x00\xc2\x00\x00\x00\x00\x00\xa0\x00\xa0\x00\x00\x00\x00\x00\xb9\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xb4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcb\x00\x2a\x00\x00\x00\x00\x00\xba\x00\xbf\x00\xbe\x00\x00\x00\x00\x00\x08\x00\x10\x01\x4f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x01\x00\x00\x00\x00\x00\x00\x00\x00\xe2\x00\x00\x00\x00\x00\x00\x00\x30\x01\x00\x00\x00\x00\x37\x01\x00\x00\x7e\x00\x4e\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd7\x00\x00\x00\x00\x00\x55\x01\x90\x01\x00\x00\x8a\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x94\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x92\x00\x6c\x01\x00\x00\x00\x00\x00\x00\x00\x00\x7a\x00\xb5\x00\x73\x01\x00\x00\xd0\x00\x00\x00\xd2\x00\x00\x00\x00\x00\xe0\x00\x00\x00\x00\x00\xf7\x00\x00\x00\x00\x00\x00\x00\xef\x00\xe3\x00\x00\x00\x00\x00\x00\x00\xf9\x00\xfc\x00\x00\x00\xf2\x00\xcc\x00\xf8\x00\x00\x00\xf5\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9c\x00\x00\x00\x00\x00\xfd\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\x00\x0a\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xee\xff\x00\x00\xfe\xff\xd6\xff\xd5\xff\xd1\xff\x00\x00\xe2\xff\xeb\xff\xfb\xff\xee\xff\x00\x00\xcd\xff\xcb\xff\xc9\xff\xc7\xff\xc5\xff\xc3\xff\x00\x00\x00\x00\xf2\xff\x00\x00\xec\xff\xdc\xff\xd3\xff\x00\x00\xde\xff\x00\x00\x00\x00\xdd\xff\xd4\xff\xdb\xff\x00\x00\xee\xff\xfd\xff\xfc\xff\x00\x00\x00\x00\xd6\xff\x00\x00\x00\x00\xe7\xff\xf7\xff\x00\x00\xcf\xff\xf1\xff\x00\x00\xf4\xff\x00\x00\x00\x00\xce\xff\xe3\xff\x00\x00\x00\x00\xb4\xff\x00\x00\xb9\xff\xb8\xff\xb5\xff\xb7\xff\xb6\xff\x00\x00\xbe\xff\xbd\xff\x00\x00\xba\xff\xbc\xff\xbb\xff\x00\x00\xe0\xff\x00\x00\xed\xff\xc1\xff\x00\x00\x00\x00\xc0\xff\x00\x00\xea\xff\x00\x00\xf7\xff\x00\x00\xcc\xff\xca\xff\xc6\xff\xc8\xff\xc4\xff\xf3\xff\xf2\xff\xd0\xff\x00\x00\xf2\xff\x00\x00\xf6\xff\x00\x00\x00\x00\x00\x00\xe8\xff\x00\x00\xef\xff\x00\x00\x00\x00\xf9\xff\x00\x00\xf7\xff\x00\x00\xf0\xff\x00\x00\xe1\xff\x00\x00\x00\x00\xdf\xff\xd2\xff\xc1\xff\xe9\xff\xbf\xff\x00\x00\xda\xff\x00\x00\xf5\xff\x00\x00\xf8\xff\xe4\xff\x00\x00\x00\x00\xe6\xff\xfa\xff\x00\x00\xc2\xff\xe5\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x03\x00\x01\x00\x00\x00\x01\x00\x02\x00\x08\x00\x06\x00\x00\x00\x01\x00\x02\x00\x0d\x00\x0b\x00\x24\x00\x09\x00\x0e\x00\x0b\x00\x06\x00\x27\x00\x10\x00\x11\x00\x12\x00\x15\x00\x24\x00\x10\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x18\x00\x06\x00\x24\x00\x25\x00\x26\x00\x01\x00\x1e\x00\x00\x00\x04\x00\x22\x00\x06\x00\x06\x00\x24\x00\x25\x00\x26\x00\x0b\x00\x15\x00\x0e\x00\x06\x00\x0c\x00\x0d\x00\x1a\x00\x11\x00\x06\x00\x1d\x00\x15\x00\x1f\x00\x06\x00\x18\x00\x11\x00\x1a\x00\x1b\x00\x06\x00\x1d\x00\x1e\x00\x1f\x00\x16\x00\x17\x00\x01\x00\x06\x00\x24\x00\x25\x00\x26\x00\x06\x00\x00\x00\x01\x00\x02\x00\x07\x00\x0b\x00\x01\x00\x23\x00\x0e\x00\x02\x00\x01\x00\x06\x00\x05\x00\x0e\x00\x0a\x00\x06\x00\x0b\x00\x10\x00\x18\x00\x07\x00\x0b\x00\x1b\x00\x0f\x00\x10\x00\x1e\x00\x12\x00\x13\x00\x14\x00\x0a\x00\x18\x00\x24\x00\x25\x00\x26\x00\x18\x00\x0e\x00\x1e\x00\x1b\x00\x0e\x00\x04\x00\x1e\x00\x06\x00\x24\x00\x25\x00\x26\x00\x00\x00\x24\x00\x25\x00\x26\x00\x0a\x00\x04\x00\x04\x00\x06\x00\x06\x00\x05\x00\x06\x00\x15\x00\x0c\x00\x0d\x00\x07\x00\x0c\x00\x1a\x00\x24\x00\x0e\x00\x1d\x00\x07\x00\x1f\x00\x15\x00\x15\x00\x00\x00\x01\x00\x02\x00\x1a\x00\x1a\x00\x0a\x00\x1d\x00\x1d\x00\x1f\x00\x1f\x00\x00\x00\x01\x00\x02\x00\x07\x00\x07\x00\x21\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x24\x00\x07\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x24\x00\x05\x00\x06\x00\x11\x00\x09\x00\x0a\x00\x0b\x00\x21\x00\x21\x00\x0e\x00\x0e\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x0c\x00\x04\x00\x17\x00\x05\x00\x06\x00\x19\x00\x09\x00\x0a\x00\x0b\x00\x18\x00\x1a\x00\x0e\x00\x0e\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x0e\x00\x04\x00\x07\x00\x08\x00\x19\x00\x18\x00\x09\x00\x0a\x00\x0b\x00\x0e\x00\x00\x00\x0e\x00\x0e\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x00\x00\x04\x00\x07\x00\x08\x00\x0e\x00\x09\x00\x09\x00\x0e\x00\x0b\x00\x0e\x00\x09\x00\x0e\x00\x09\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\x09\x00\xff\xff\xff\xff\xff\xff\x07\x00\x08\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\x0e\x00\xff\xff\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\xff\xff\xff\xff\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\xff\xff\xff\xff\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x10\x00\x11\x00\x12\x00\x13\x00\x10\x00\x11\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x42\x00\x14\x00\x26\x00\x04\x00\x05\x00\x43\x00\x15\x00\x26\x00\x04\x00\x05\x00\x44\x00\x16\x00\x03\x00\x3f\x00\x17\x00\x40\x00\x29\x00\xff\xff\x0c\x00\x0d\x00\x52\x00\x18\x00\x03\x00\x32\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x19\x00\x58\x00\x03\x00\x23\x00\x24\x00\x14\x00\x1f\x00\x44\x00\x32\x00\x35\x00\x15\x00\x49\x00\x03\x00\x23\x00\x24\x00\x16\x00\x18\x00\x34\x00\x50\x00\x45\x00\x46\x00\x1b\x00\x4a\x00\x2c\x00\x1e\x00\x18\x00\x20\x00\x2b\x00\x19\x00\x51\x00\x1b\x00\x1c\x00\x26\x00\x1e\x00\x1f\x00\x20\x00\x7c\x00\x7d\x00\x14\x00\x49\x00\x03\x00\x23\x00\x24\x00\x29\x00\x26\x00\x04\x00\x05\x00\x5a\x00\x16\x00\x14\x00\x63\x00\x2a\x00\x37\x00\x14\x00\x29\x00\x38\x00\x61\x00\x5b\x00\x29\x00\x16\x00\x2c\x00\x19\x00\x59\x00\x16\x00\x1c\x00\x39\x00\x3a\x00\x1f\x00\x3b\x00\x3c\x00\x3d\x00\x4f\x00\x19\x00\x03\x00\x23\x00\x24\x00\x19\x00\x4e\x00\x1f\x00\x1c\x00\x72\x00\x60\x00\x1f\x00\x58\x00\x03\x00\x23\x00\x24\x00\x6d\x00\x03\x00\x23\x00\x24\x00\x71\x00\x32\x00\x60\x00\x58\x00\x58\x00\x5c\x00\x5d\x00\x18\x00\x45\x00\x6e\x00\x70\x00\x6b\x00\x1b\x00\x03\x00\x5e\x00\x1e\x00\x69\x00\x20\x00\x18\x00\x18\x00\x26\x00\x04\x00\x05\x00\x1b\x00\x1b\x00\x68\x00\x1e\x00\x1e\x00\x20\x00\x20\x00\x26\x00\x04\x00\x05\x00\x67\x00\x64\x00\x22\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x4b\x00\x4c\x00\x03\x00\x74\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x4b\x00\x72\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x03\x00\x5c\x00\x6c\x00\x51\x00\x08\x00\x09\x00\x0a\x00\x22\x00\x22\x00\x0b\x00\x5e\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x03\x00\x04\x00\x05\x00\x7b\x00\x07\x00\x81\x00\x5c\x00\x76\x00\x40\x00\x08\x00\x47\x00\x0a\x00\x3d\x00\x35\x00\x0b\x00\x5e\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x03\x00\x04\x00\x05\x00\x56\x00\x07\x00\x2d\x00\x2e\x00\x40\x00\x3d\x00\x08\x00\x24\x00\x0a\x00\x2f\x00\x65\x00\x0b\x00\x64\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x03\x00\x04\x00\x05\x00\x78\x00\x07\x00\x2d\x00\x69\x00\x77\x00\x75\x00\x08\x00\x74\x00\x79\x00\x2f\x00\x7d\x00\x0b\x00\x7f\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x26\x00\x04\x00\x05\x00\x7e\x00\x00\x00\x00\x00\x00\x00\x2d\x00\x2e\x00\x26\x00\x04\x00\x05\x00\x00\x00\x00\x00\x2f\x00\x00\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x30\x00\x00\x00\x00\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x27\x00\x26\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x61\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x30\x00\x26\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x5b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x55\x00\x26\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x4a\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x6b\x00\x26\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x26\x00\x04\x00\x05\x00\x00\x00\x26\x00\x04\x00\x05\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x53\x00\x0c\x00\x0d\x00\x0e\x00\x54\x00\x0c\x00\x51\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 75) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75)
	]

happy_n_terms = 40 :: Prelude.Int
happy_n_nonterms = 27 :: Prelude.Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn4
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.Ident (tokenText happy_var_1))
	)}

happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn5
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), (read (tokenText happy_var_1)) :: Integer)
	)}

happyReduce_3 = happySpecReduce_1  2# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn6
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), ((\(PT _ (TL s)) -> s) happy_var_1))
	)}

happyReduce_4 = happySpecReduce_1  3# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOut14 happy_x_1 of { (HappyWrap14 happy_var_1) -> 
	happyIn7
		 ((fst happy_var_1, AbsGramatyka.SProgram (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_5 = happyReduce 6# 4# happyReduction_5
happyReduction_5 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut4 happy_x_2 of { (HappyWrap4 happy_var_2) -> 
	case happyOut10 happy_x_4 of { (HappyWrap10 happy_var_4) -> 
	case happyOut13 happy_x_6 of { (HappyWrap13 happy_var_6) -> 
	happyIn8
		 ((fst happy_var_1, AbsGramatyka.PFnDef (fst happy_var_1) (snd happy_var_1) (snd happy_var_2) (snd happy_var_4) (snd happy_var_6))
	) `HappyStk` happyRest}}}}

happyReduce_6 = happySpecReduce_2  5# happyReduction_6
happyReduction_6 happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut4 happy_x_2 of { (HappyWrap4 happy_var_2) -> 
	happyIn9
		 ((fst happy_var_1, AbsGramatyka.VArg (fst happy_var_1) (snd happy_var_1) (snd happy_var_2))
	)}}

happyReduce_7 = happySpecReduce_3  5# happyReduction_7
happyReduction_7 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_2 of { (HappyWrap18 happy_var_2) -> 
	case happyOut4 happy_x_3 of { (HappyWrap4 happy_var_3) -> 
	happyIn9
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.PArg (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_3))
	)}}}

happyReduce_8 = happySpecReduce_0  6# happyReduction_8
happyReduction_8  =  happyIn10
		 ((AbsGramatyka.BNFC'NoPosition, [])
	)

happyReduce_9 = happySpecReduce_1  6# happyReduction_9
happyReduction_9 happy_x_1
	 =  case happyOut9 happy_x_1 of { (HappyWrap9 happy_var_1) -> 
	happyIn10
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_10 = happySpecReduce_3  6# happyReduction_10
happyReduction_10 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut9 happy_x_1 of { (HappyWrap9 happy_var_1) -> 
	case happyOut10 happy_x_3 of { (HappyWrap10 happy_var_3) -> 
	happyIn10
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_11 = happySpecReduce_1  7# happyReduction_11
happyReduction_11 happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	happyIn11
		 ((fst happy_var_1, AbsGramatyka.VArgType (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_12 = happySpecReduce_2  7# happyReduction_12
happyReduction_12 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_2 of { (HappyWrap18 happy_var_2) -> 
	happyIn11
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.PArgType (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_13 = happySpecReduce_0  8# happyReduction_13
happyReduction_13  =  happyIn12
		 ((AbsGramatyka.BNFC'NoPosition, [])
	)

happyReduce_14 = happySpecReduce_1  8# happyReduction_14
happyReduction_14 happy_x_1
	 =  case happyOut11 happy_x_1 of { (HappyWrap11 happy_var_1) -> 
	happyIn12
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_15 = happySpecReduce_3  8# happyReduction_15
happyReduction_15 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { (HappyWrap11 happy_var_1) -> 
	case happyOut12 happy_x_3 of { (HappyWrap12 happy_var_3) -> 
	happyIn12
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_16 = happySpecReduce_3  9# happyReduction_16
happyReduction_16 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut14 happy_x_2 of { (HappyWrap14 happy_var_2) -> 
	happyIn13
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.SBlock (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_17 = happySpecReduce_0  10# happyReduction_17
happyReduction_17  =  happyIn14
		 ((AbsGramatyka.BNFC'NoPosition, [])
	)

happyReduce_18 = happySpecReduce_2  10# happyReduction_18
happyReduction_18 happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
	case happyOut14 happy_x_2 of { (HappyWrap14 happy_var_2) -> 
	happyIn14
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_2))
	)}}

happyReduce_19 = happySpecReduce_1  11# happyReduction_19
happyReduction_19 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn15
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.Empty (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_20 = happySpecReduce_1  11# happyReduction_20
happyReduction_20 happy_x_1
	 =  case happyOut13 happy_x_1 of { (HappyWrap13 happy_var_1) -> 
	happyIn15
		 ((fst happy_var_1, AbsGramatyka.BStmt (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_21 = happySpecReduce_3  11# happyReduction_21
happyReduction_21 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut17 happy_x_2 of { (HappyWrap17 happy_var_2) -> 
	happyIn15
		 ((fst happy_var_1, AbsGramatyka.Decl (fst happy_var_1) (snd happy_var_1) (snd happy_var_2))
	)}}

happyReduce_22 = happyReduce 4# 11# happyReduction_22
happyReduction_22 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	case happyOut26 happy_x_3 of { (HappyWrap26 happy_var_3) -> 
	happyIn15
		 ((fst happy_var_1, AbsGramatyka.Ass (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_23 = happySpecReduce_3  11# happyReduction_23
happyReduction_23 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_2 of { (HappyWrap26 happy_var_2) -> 
	happyIn15
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.Ret (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_24 = happySpecReduce_2  11# happyReduction_24
happyReduction_24 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn15
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.VRet (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_25 = happyReduce 6# 11# happyReduction_25
happyReduction_25 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_3 of { (HappyWrap26 happy_var_3) -> 
	case happyOut13 happy_x_5 of { (HappyWrap13 happy_var_5) -> 
	happyIn15
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.Cond (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_3) (snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_26 = happyReduce 8# 11# happyReduction_26
happyReduction_26 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_3 of { (HappyWrap26 happy_var_3) -> 
	case happyOut13 happy_x_5 of { (HappyWrap13 happy_var_5) -> 
	case happyOut13 happy_x_7 of { (HappyWrap13 happy_var_7) -> 
	happyIn15
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.CondElse (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_3) (snd happy_var_5) (snd happy_var_7))
	) `HappyStk` happyRest}}}}

happyReduce_27 = happyReduce 5# 11# happyReduction_27
happyReduction_27 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_3 of { (HappyWrap26 happy_var_3) -> 
	case happyOut15 happy_x_5 of { (HappyWrap15 happy_var_5) -> 
	happyIn15
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.While (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_3) (snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_28 = happySpecReduce_2  11# happyReduction_28
happyReduction_28 happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { (HappyWrap26 happy_var_1) -> 
	happyIn15
		 ((fst happy_var_1, AbsGramatyka.SExp (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_29 = happySpecReduce_1  11# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
	happyIn15
		 ((fst happy_var_1, AbsGramatyka.FunExp (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_30 = happySpecReduce_3  12# happyReduction_30
happyReduction_30 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	case happyOut26 happy_x_3 of { (HappyWrap26 happy_var_3) -> 
	happyIn16
		 ((fst happy_var_1, AbsGramatyka.Init (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_31 = happySpecReduce_1  13# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOut16 happy_x_1 of { (HappyWrap16 happy_var_1) -> 
	happyIn17
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_32 = happySpecReduce_3  13# happyReduction_32
happyReduction_32 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { (HappyWrap16 happy_var_1) -> 
	case happyOut17 happy_x_3 of { (HappyWrap17 happy_var_3) -> 
	happyIn17
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_33 = happySpecReduce_1  14# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn18
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.Int (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_34 = happySpecReduce_1  14# happyReduction_34
happyReduction_34 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn18
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.Str (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_35 = happySpecReduce_1  14# happyReduction_35
happyReduction_35 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn18
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.Bool (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_36 = happySpecReduce_1  14# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn18
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.Void (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_37 = happyReduce 5# 14# happyReduction_37
happyReduction_37 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_2 of { (HappyWrap12 happy_var_2) -> 
	case happyOut18 happy_x_5 of { (HappyWrap18 happy_var_5) -> 
	happyIn18
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.Fun (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_38 = happySpecReduce_0  15# happyReduction_38
happyReduction_38  =  happyIn19
		 ((AbsGramatyka.BNFC'NoPosition, [])
	)

happyReduce_39 = happySpecReduce_1  15# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	happyIn19
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_40 = happySpecReduce_3  15# happyReduction_40
happyReduction_40 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut19 happy_x_3 of { (HappyWrap19 happy_var_3) -> 
	happyIn19
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_41 = happySpecReduce_1  16# happyReduction_41
happyReduction_41 happy_x_1
	 =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	happyIn20
		 ((fst happy_var_1, AbsGramatyka.EVar (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_42 = happySpecReduce_1  16# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOut5 happy_x_1 of { (HappyWrap5 happy_var_1) -> 
	happyIn20
		 ((fst happy_var_1, AbsGramatyka.ELitInt (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_43 = happySpecReduce_1  16# happyReduction_43
happyReduction_43 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn20
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.ELitTrue (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_44 = happySpecReduce_1  16# happyReduction_44
happyReduction_44 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn20
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.ELitFalse (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_45 = happyReduce 4# 16# happyReduction_45
happyReduction_45 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	case happyOut27 happy_x_3 of { (HappyWrap27 happy_var_3) -> 
	happyIn20
		 ((fst happy_var_1, AbsGramatyka.EApp (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_46 = happySpecReduce_1  16# happyReduction_46
happyReduction_46 happy_x_1
	 =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	happyIn20
		 ((fst happy_var_1, AbsGramatyka.EString (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_47 = happySpecReduce_3  16# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_2 of { (HappyWrap26 happy_var_2) -> 
	happyIn20
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), (snd happy_var_2))
	)}}

happyReduce_48 = happySpecReduce_2  17# happyReduction_48
happyReduction_48 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut20 happy_x_2 of { (HappyWrap20 happy_var_2) -> 
	happyIn21
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.Neg (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_49 = happySpecReduce_2  17# happyReduction_49
happyReduction_49 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut20 happy_x_2 of { (HappyWrap20 happy_var_2) -> 
	happyIn21
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.Not (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_50 = happySpecReduce_1  17# happyReduction_50
happyReduction_50 happy_x_1
	 =  case happyOut20 happy_x_1 of { (HappyWrap20 happy_var_1) -> 
	happyIn21
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_51 = happySpecReduce_3  18# happyReduction_51
happyReduction_51 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { (HappyWrap22 happy_var_1) -> 
	case happyOut29 happy_x_2 of { (HappyWrap29 happy_var_2) -> 
	case happyOut21 happy_x_3 of { (HappyWrap21 happy_var_3) -> 
	happyIn22
		 ((fst happy_var_1, AbsGramatyka.EMul (fst happy_var_1) (snd happy_var_1) (snd happy_var_2) (snd happy_var_3))
	)}}}

happyReduce_52 = happySpecReduce_1  18# happyReduction_52
happyReduction_52 happy_x_1
	 =  case happyOut21 happy_x_1 of { (HappyWrap21 happy_var_1) -> 
	happyIn22
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_53 = happySpecReduce_3  19# happyReduction_53
happyReduction_53 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { (HappyWrap23 happy_var_1) -> 
	case happyOut28 happy_x_2 of { (HappyWrap28 happy_var_2) -> 
	case happyOut22 happy_x_3 of { (HappyWrap22 happy_var_3) -> 
	happyIn23
		 ((fst happy_var_1, AbsGramatyka.EAdd (fst happy_var_1) (snd happy_var_1) (snd happy_var_2) (snd happy_var_3))
	)}}}

happyReduce_54 = happySpecReduce_1  19# happyReduction_54
happyReduction_54 happy_x_1
	 =  case happyOut22 happy_x_1 of { (HappyWrap22 happy_var_1) -> 
	happyIn23
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_55 = happySpecReduce_3  20# happyReduction_55
happyReduction_55 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
	case happyOut30 happy_x_2 of { (HappyWrap30 happy_var_2) -> 
	case happyOut23 happy_x_3 of { (HappyWrap23 happy_var_3) -> 
	happyIn24
		 ((fst happy_var_1, AbsGramatyka.ERel (fst happy_var_1) (snd happy_var_1) (snd happy_var_2) (snd happy_var_3))
	)}}}

happyReduce_56 = happySpecReduce_1  20# happyReduction_56
happyReduction_56 happy_x_1
	 =  case happyOut23 happy_x_1 of { (HappyWrap23 happy_var_1) -> 
	happyIn24
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_57 = happySpecReduce_3  21# happyReduction_57
happyReduction_57 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
	case happyOut25 happy_x_3 of { (HappyWrap25 happy_var_3) -> 
	happyIn25
		 ((fst happy_var_1, AbsGramatyka.EAnd (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_58 = happySpecReduce_1  21# happyReduction_58
happyReduction_58 happy_x_1
	 =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
	happyIn25
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_59 = happySpecReduce_3  22# happyReduction_59
happyReduction_59 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { (HappyWrap25 happy_var_1) -> 
	case happyOut26 happy_x_3 of { (HappyWrap26 happy_var_3) -> 
	happyIn26
		 ((fst happy_var_1, AbsGramatyka.EOr (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_60 = happySpecReduce_1  22# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOut25 happy_x_1 of { (HappyWrap25 happy_var_1) -> 
	happyIn26
		 ((fst happy_var_1, (snd happy_var_1))
	)}

happyReduce_61 = happyReduce 7# 22# happyReduction_61
happyReduction_61 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_3 of { (HappyWrap10 happy_var_3) -> 
	case happyOut18 happy_x_5 of { (HappyWrap18 happy_var_5) -> 
	case happyOut13 happy_x_7 of { (HappyWrap13 happy_var_7) -> 
	happyIn26
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.ELam (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)) (snd happy_var_3) (snd happy_var_5) (snd happy_var_7))
	) `HappyStk` happyRest}}}}

happyReduce_62 = happySpecReduce_0  23# happyReduction_62
happyReduction_62  =  happyIn27
		 ((AbsGramatyka.BNFC'NoPosition, [])
	)

happyReduce_63 = happySpecReduce_1  23# happyReduction_63
happyReduction_63 happy_x_1
	 =  case happyOut26 happy_x_1 of { (HappyWrap26 happy_var_1) -> 
	happyIn27
		 ((fst happy_var_1, (:[]) (snd happy_var_1))
	)}

happyReduce_64 = happySpecReduce_3  23# happyReduction_64
happyReduction_64 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { (HappyWrap26 happy_var_1) -> 
	case happyOut27 happy_x_3 of { (HappyWrap27 happy_var_3) -> 
	happyIn27
		 ((fst happy_var_1, (:) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_65 = happySpecReduce_1  24# happyReduction_65
happyReduction_65 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn28
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.Plus (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_66 = happySpecReduce_1  24# happyReduction_66
happyReduction_66 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn28
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.Minus (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_67 = happySpecReduce_1  25# happyReduction_67
happyReduction_67 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn29
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.Times (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_68 = happySpecReduce_1  25# happyReduction_68
happyReduction_68 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn29
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.Div (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_69 = happySpecReduce_1  25# happyReduction_69
happyReduction_69 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn29
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.Mod (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_70 = happySpecReduce_1  26# happyReduction_70
happyReduction_70 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn30
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.LTH (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_71 = happySpecReduce_1  26# happyReduction_71
happyReduction_71 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn30
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.LE (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_72 = happySpecReduce_1  26# happyReduction_72
happyReduction_72 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn30
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.GTH (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_73 = happySpecReduce_1  26# happyReduction_73
happyReduction_73 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn30
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.GE (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_74 = happySpecReduce_1  26# happyReduction_74
happyReduction_74 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn30
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.EQU (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyReduce_75 = happySpecReduce_1  26# happyReduction_75
happyReduction_75 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn30
		 ((uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1), AbsGramatyka.NE (uncurry AbsGramatyka.BNFC'Position (tokenLineCol happy_var_1)))
	)}

happyNewToken action sts stk [] =
	happyDoAction 39# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TS _ 34) -> cont 34#;
	PT _ (TS _ 35) -> cont 35#;
	PT _ (TV _) -> cont 36#;
	PT _ (TI _) -> cont 37#;
	PT _ (TL _) -> cont 38#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 39# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = ((>>=))
happyReturn :: () => a -> Err a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProgram_internal tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap7 x') = happyOut7 x} in x'))

happySeq = happyDontSeq


type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

-- Entrypoints

pProgram :: [Token] -> Err AbsGramatyka.Program
pProgram = fmap snd . pProgram_internal
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Prelude.Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else Prelude.False
         action
          | check     = indexShortOffAddr happyTable off_i
          | Prelude.otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `Prelude.mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ((Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
