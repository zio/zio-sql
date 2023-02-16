package zio.sql.select

import zio.sql.expr.Expr
import zio.sql.select._

// format: off

final case class SelectByCommaBuilder() {

  def apply[F1, Source, B1](expr1: Expr[F1, Source, B1]) = {
    SelectBuilder[F1, Source, SelectionSet.Cons[Source, B1, SelectionSet.Empty]](expr1)
  }

  def apply[F1, F2, Source, B1, B2](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2]) = {
      val selection = expr1 ++ expr2
      SelectBuilder[F1 with F2, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Empty]]](selection)
  }

  def apply[F1, F2, F3, Source, B1, B2, B3](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3]) = {
      val selection = expr1 ++ expr2 ++ expr3
      SelectBuilder[F1 with F2 with F3, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Empty]]]](selection)
  }

  def apply[F1, F2, F3, F4, Source, B1, B2, B3, B4](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4]) = {
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4
      SelectBuilder[F1 with F2 with F3 with F4, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Empty]]]]](selection)
  }

  def apply[F1, F2, F3, F4, F5, Source, B1, B2, B3, B4, B5](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5]) = {
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5
      SelectBuilder[F1 with F2 with F3 with F4 with F5, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Empty]]]]]](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, Source, B1, B2, B3, B4, B5, B6](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6]) = {
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6
      SelectBuilder[F1 with F2 with F3 with F4 with F5 with F6, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Empty]]]]]]](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, Source, B1, B2, B3, B4, B5, B6, B7](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7]) = {
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7
      SelectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Empty]]]]]]]](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, Source, B1, B2, B3, B4, B5, B6, B7, B8](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8]) = {
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8
      SelectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 , Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Empty]]]]]]]]](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9]) = {
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9
      SelectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 , Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Empty]]]]]]]]]](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10]) = {
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10
      SelectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 , Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Empty]]]]]]]]]]](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11]) = {
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11
      SelectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Empty]]]]]]]]]]]](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12]) = {
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12
      SelectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Empty]]]]]]]]]]]]](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13]) = {

      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13
      SelectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Empty]]]]]]]]]]]]]](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14]) = {

      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14
      SelectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Empty]]]]]]]]]]]]]]](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15]) = {

      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15
      SelectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Empty]]]]]]]]]]]]]]]](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16]) = {

      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16
      SelectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Empty]]]]]]]]]]]]]]]]](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17]) = {

      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17
      SelectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Empty]]]]]]]]]]]]]]]]]](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17], expr18: Expr[F18, Source, B18]) = {
      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18
      SelectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Cons[Source, B18, SelectionSet.Empty]]]]]]]]]]]]]]]]]]](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17], expr18: Expr[F18, Source, B18], expr19: Expr[F19, Source, B19]) = {

      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19
      SelectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Cons[Source, B18, SelectionSet.Cons[Source, B19, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17], expr18: Expr[F18, Source, B18], expr19: Expr[F19, Source, B19], expr20: Expr[F20, Source, B20]) = {

      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19 ++ expr20
      SelectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Cons[Source, B18, SelectionSet.Cons[Source, B19, SelectionSet.Cons[Source, B20, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]]](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20, F21, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20, B21](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17], expr18: Expr[F18, Source, B18], expr19: Expr[F19, Source, B19], expr20: Expr[F20, Source, B20], expr21: Expr[F21, Source, B21])= {

      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19 ++ expr20 ++ expr21
      SelectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20 with F21, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Cons[Source, B18, SelectionSet.Cons[Source, B19, SelectionSet.Cons[Source, B20, SelectionSet.Cons[Source, B21, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]]]](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20, F21, F22, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20, B21, B22](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17], expr18: Expr[F18, Source, B18], expr19: Expr[F19, Source, B19], expr20: Expr[F20, Source, B20], expr21: Expr[F21, Source, B21], expr22: Expr[F22, Source, B22]) = {

      val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19 ++ expr20 ++ expr21 ++ expr22
      SelectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20 with F21 with F22, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Cons[Source, B18, SelectionSet.Cons[Source, B19, SelectionSet.Cons[Source, B20, SelectionSet.Cons[Source, B21, SelectionSet.Cons[Source, B22, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]]]]](selection)
  }
}

final class SubselectPartiallyApplied[ParentTable] {
  def apply[F, A, B <: SelectionSet[A]](selection: Selection[F, A, B]) =
    SubselectBuilder[F, A, B, ParentTable](selection)

  def apply[F1, F2, Source, B1, B2](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2]) = {
    val selection = expr1 ++ expr2
    SubselectBuilder[F1 with F2, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Empty]], ParentTable](selection)
  }

  def apply[F1, F2, F3, Source, B1, B2, B3](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3]) = {
    val selection = expr1 ++ expr2 ++ expr3
    SubselectBuilder[F1 with F2 with F3, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Empty]]], ParentTable](selection)
  }

  def apply[F1, F2, F3, F4, Source, B1, B2, B3, B4](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4]) = {
    val selection = expr1 ++ expr2 ++ expr3 ++ expr4
    SubselectBuilder[F1 with F2 with F3 with F4, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Empty]]]], ParentTable](selection)
  }

  def apply[F1, F2, F3, F4, F5, Source, B1, B2, B3, B4, B5](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5]) = {
    val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5
    SubselectBuilder[F1 with F2 with F3 with F4 with F5, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Empty]]]]], ParentTable](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, Source, B1, B2, B3, B4, B5, B6](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6]) = {
    val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6
    SubselectBuilder[F1 with F2 with F3 with F4 with F5 with F6, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Empty]]]]]], ParentTable](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, Source, B1, B2, B3, B4, B5, B6, B7](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7]) = {
    val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7
    SubselectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Empty]]]]]]], ParentTable](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, Source, B1, B2, B3, B4, B5, B6, B7, B8](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8]) = {
    
    val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8
    SubselectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Empty]]]]]]]], ParentTable](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9]) = {
    val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9
    SubselectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Empty]]]]]]]]], ParentTable](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10]) = {

    val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10
    SubselectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Empty]]]]]]]]]], ParentTable](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11]) = {

    val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11
    SubselectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Empty]]]]]]]]]]], ParentTable](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12]) = {

    val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12
    SubselectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Empty]]]]]]]]]]]], ParentTable](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13]) = {

    val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13
    SubselectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Empty]]]]]]]]]]]]], ParentTable](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14]) = {
    
    val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14
    SubselectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Empty]]]]]]]]]]]]]], ParentTable](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15]) = {

    val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15
    SubselectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Empty]]]]]]]]]]]]]]], ParentTable](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16]) = {

    val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16
    SubselectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Empty]]]]]]]]]]]]]]]], ParentTable](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17]) = {

    val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17
    SubselectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Empty]]]]]]]]]]]]]]]]], ParentTable](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17], expr18: Expr[F18, Source, B18]) = {

    val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18
    SubselectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Cons[Source, B18, SelectionSet.Empty]]]]]]]]]]]]]]]]]], ParentTable](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17], expr18: Expr[F18, Source, B18], expr19: Expr[F19, Source, B19]) = {

    val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19
    SubselectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 , Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Cons[Source, B18, SelectionSet.Cons[Source, B19, SelectionSet.Empty]]]]]]]]]]]]]]]]]]], ParentTable](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17], expr18: Expr[F18, Source, B18], expr19: Expr[F19, Source, B19], expr20: Expr[F20, Source, B20]) = {

    val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19 ++ expr20
    SubselectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Cons[Source, B18, SelectionSet.Cons[Source, B19, SelectionSet.Cons[Source, B20, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]], ParentTable](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20, F21, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20, B21](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17], expr18: Expr[F18, Source, B18], expr19: Expr[F19, Source, B19], expr20: Expr[F20, Source, B20], expr21: Expr[F21, Source, B21]) = {

    val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19 ++ expr20 ++ expr21
    SubselectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20 with F21, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Cons[Source, B18, SelectionSet.Cons[Source, B19, SelectionSet.Cons[Source, B20, SelectionSet.Cons[Source, B21, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]]], ParentTable](selection)
  }

  def apply[F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16, F17, F18, F19, F20, F21, F22, Source, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B20, B21, B22](expr1: Expr[F1, Source, B1], expr2: Expr[F2, Source, B2], expr3: Expr[F3, Source, B3], expr4: Expr[F4, Source, B4], expr5: Expr[F5, Source, B5], expr6: Expr[F6, Source, B6], expr7: Expr[F7, Source, B7], expr8: Expr[F8, Source, B8], expr9: Expr[F9, Source, B9], expr10: Expr[F10, Source, B10], expr11: Expr[F11, Source, B11], expr12: Expr[F12, Source, B12], expr13: Expr[F13, Source, B13], expr14: Expr[F14, Source, B14], expr15: Expr[F15, Source, B15], expr16: Expr[F16, Source, B16], expr17: Expr[F17, Source, B17], expr18: Expr[F18, Source, B18], expr19: Expr[F19, Source, B19], expr20: Expr[F20, Source, B20], expr21: Expr[F21, Source, B21], expr22: Expr[F22, Source, B22]) = {
    
    val selection = expr1 ++ expr2 ++ expr3 ++ expr4 ++ expr5 ++ expr6 ++ expr7 ++ expr8 ++ expr9 ++ expr10 ++ expr11 ++ expr12 ++ expr13 ++ expr14 ++ expr15 ++ expr16 ++ expr17 ++ expr18 ++ expr19 ++ expr20 ++ expr21 ++ expr22
    SubselectBuilder[F1 with F2 with F3 with F4 with F5 with F6 with F7 with F8 with F9 with F10 with F11 with F12 with F13 with F14 with F15 with F16 with F17 with F18 with F19 with F20 with F21 with F22, Source, SelectionSet.Cons[Source, B1, SelectionSet.Cons[Source, B2, SelectionSet.Cons[Source, B3, SelectionSet.Cons[Source, B4, SelectionSet.Cons[Source, B5, SelectionSet.Cons[Source, B6, SelectionSet.Cons[Source, B7, SelectionSet.Cons[Source, B8, SelectionSet.Cons[Source, B9, SelectionSet.Cons[Source, B10, SelectionSet.Cons[Source, B11, SelectionSet.Cons[Source, B12, SelectionSet.Cons[Source, B13, SelectionSet.Cons[Source, B14, SelectionSet.Cons[Source, B15, SelectionSet.Cons[Source, B16, SelectionSet.Cons[Source, B17, SelectionSet.Cons[Source, B18, SelectionSet.Cons[Source, B19, SelectionSet.Cons[Source, B20, SelectionSet.Cons[Source, B21, SelectionSet.Cons[Source, B22, SelectionSet.Empty]]]]]]]]]]]]]]]]]]]]]], ParentTable](selection)
  }  
}
// format: on
