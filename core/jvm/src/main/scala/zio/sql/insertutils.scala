package zio.sql

import zio.schema.Schema

trait InsertUtilsModule { self: FeaturesModule =>

  sealed trait SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source]

  // format: off
  object SchemaValidity extends SchemaValidityCaseClasses {
     implicit def tuple1[F, A1, ColsRepr,  AllColumnIdentities, Source, Identity1](implicit
      ev1: Schema[A1],
      ev2: (A1, Unit) <:< ColsRepr,
      ev3: F <:< Features.Source[Identity1, Source],
      ev4: AllColumnIdentities <:< Identity1
    ): SchemaValidity[F, A1, ColsRepr, AllColumnIdentities, Source] =
      new SchemaValidity[F, A1, ColsRepr, AllColumnIdentities, Source] {}


    implicit def tuple2[F, A1, A2, ColsRepr,  AllColumnIdentities, Source, Identity1, Identity2](implicit
      ev1: Schema[(A1, A2)],
      ev2: (A1, (A2, Unit)) <:< ColsRepr,
      ev3: F <:< Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]],
      ev4: AllColumnIdentities <:< Identity1 with Identity2
    ): SchemaValidity[F, (A1, A2), ColsRepr, AllColumnIdentities, Source] =
      new SchemaValidity[F, (A1, A2), ColsRepr, AllColumnIdentities, Source] {}

    implicit def tuple3[F, A1, A2, A3, ColsRepr,  AllColumnIdentities, Source, Identity1, Identity2, Identity3, Z](implicit
      ev1: Schema[(A1, A2, A3)],
      ev2: (A1, (A2, (A3, Unit))) <:< ColsRepr,
      ev3: F <:< Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]],
      ev4: AllColumnIdentities <:< Identity1 with Identity2 with Identity3
    ): SchemaValidity[F, (A1, A2, A3), ColsRepr, AllColumnIdentities, Source] =
      new SchemaValidity[F, (A1, A2, A3), ColsRepr, AllColumnIdentities, Source] {}

    implicit def tuple4[F, A1, A2, A3, A4, ColsRepr,  AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4](implicit
      ev1: Schema[(A1, A2, A3, A4)],
      ev2: (A1, (A2, (A3, (A4, Unit)))) <:< ColsRepr,
      ev3: F <:< Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],
      ev4: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4
    ): SchemaValidity[F, (A1, A2, A3, A4), ColsRepr, AllColumnIdentities, Source] =
      new SchemaValidity[F, (A1, A2, A3, A4), ColsRepr, AllColumnIdentities, Source] {}

    implicit def tuple5[F, A1, A2, A3, A4, A5, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5]
      (implicit 
        ev1: Schema[(A1, A2, A3, A4, A5)],
        ev2: (A1, (A2, (A3, (A4, (A5, Unit))))) <:< ColsRepr,
        ev3: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]],
        ev4: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5
      ): SchemaValidity[F, (A1, A2, A3, A4, A5), ColsRepr, AllColumnIdentities, Source] =
        new SchemaValidity[F, (A1, A2, A3, A4, A5), ColsRepr, AllColumnIdentities, Source] {}

    implicit def tuple6[F, A1, A2, A3, A4, A5, A6, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6]
      (implicit 
        ev1: Schema[(A1, A2, A3, A4, A5, A6)],
        ev2: (A1, (A2, (A3, (A4, (A5, (A6, Unit)))))) <:< ColsRepr,
        ev3: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]],
        ev4: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6
      ): SchemaValidity[F, (A1, A2, A3, A4, A5, A6), ColsRepr, AllColumnIdentities, Source] =
        new SchemaValidity[F, (A1, A2, A3, A4, A5, A6), ColsRepr, AllColumnIdentities, Source] {}

    implicit def tuple7[F, A1, A2, A3, A4, A5, A6, A7, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7]
      (implicit 
        ev1: Schema[(A1, A2, A3, A4, A5, A6, A7)],
        ev2: (A1, (A2, (A3, (A4, (A5, (A6, (A7, Unit))))))) <:< ColsRepr,
        ev3: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]],
        ev4: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7
      ): SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7), ColsRepr, AllColumnIdentities, Source] =
        new SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7), ColsRepr, AllColumnIdentities, Source] {}

    implicit def tuple8[F, A1, A2, A3, A4, A5, A6, A7, A8, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8]
      (implicit 
        ev1: Schema[(A1, A2, A3, A4, A5, A6, A7, A8)],
        ev2: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, Unit)))))))) <:< ColsRepr,
        ev3: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]],
        ev4: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8
      ): SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8), ColsRepr, AllColumnIdentities, Source] =
        new SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8), ColsRepr, AllColumnIdentities, Source] {}
  
    implicit def tuple9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9]
      (implicit 
        ev1: Schema[(A1, A2, A3, A4, A5, A6, A7, A8, A9)],
        ev2: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, Unit))))))))) <:< ColsRepr,
        ev3: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]],
        ev4: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9
      ): SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9), ColsRepr, AllColumnIdentities, Source] =
        new SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9), ColsRepr, AllColumnIdentities, Source] {}

    implicit def tuple10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10]
      (implicit 
        ev1: Schema[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)],
        ev2: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, Unit)))))))))) <:< ColsRepr,
        ev3: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]], Features.Source[Identity10, Source]],
        ev4: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10
      ): SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10), ColsRepr, AllColumnIdentities, Source] =
        new SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10), ColsRepr, AllColumnIdentities, Source] {}

    implicit def tuple11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11]
      (implicit 
        ev1: Schema[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)],
        ev2: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, Unit))))))))))) <:< ColsRepr,
        ev3: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]], Features.Source[Identity10, Source]], Features.Source[Identity11, Source]],
        ev4: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11
      ): SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11), ColsRepr, AllColumnIdentities, Source] =
        new SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11), ColsRepr, AllColumnIdentities, Source] {}

    implicit def tuple12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12]
      (implicit 
        ev1: Schema[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)],
        ev2: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, Unit)))))))))))) <:< ColsRepr,
        ev3: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]], Features.Source[Identity10, Source]], Features.Source[Identity11, Source]], Features.Source[Identity12, Source]],
        ev4: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12
      ): SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12), ColsRepr, AllColumnIdentities, Source] =
        new SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12), ColsRepr, AllColumnIdentities, Source] {}

    implicit def tuple13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13]
      (implicit 
        ev1: Schema[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)],
        ev2: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, Unit))))))))))))) <:< ColsRepr,
        ev3: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]], Features.Source[Identity10, Source]], Features.Source[Identity11, Source]], Features.Source[Identity12, Source]], Features.Source[Identity13, Source]],
        ev4: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13
      ): SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13), ColsRepr, AllColumnIdentities, Source] =
        new SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13), ColsRepr, AllColumnIdentities, Source] {}

    implicit def tuple14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14]
      (implicit 
        ev1: Schema[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)],
        ev2: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, Unit)))))))))))))) <:< ColsRepr,
        ev3: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]], Features.Source[Identity10, Source]], Features.Source[Identity11, Source]], Features.Source[Identity12, Source]], Features.Source[Identity13, Source]], Features.Source[Identity14, Source]],
        ev4: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14
      ): SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14), ColsRepr, AllColumnIdentities, Source] =
        new SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14), ColsRepr, AllColumnIdentities, Source] {}

    implicit def tuple15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14, Identity15]
      (implicit 
        ev1: Schema[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)],
        ev2: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, Unit))))))))))))))) <:< ColsRepr,
        ev3: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]], Features.Source[Identity10, Source]], Features.Source[Identity11, Source]], Features.Source[Identity12, Source]], Features.Source[Identity13, Source]], Features.Source[Identity14, Source]], Features.Source[Identity15, Source]],
        ev4: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14 with Identity15
      ): SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15), ColsRepr, AllColumnIdentities, Source] =
        new SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15), ColsRepr, AllColumnIdentities, Source] {}

    implicit def tuple16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14, Identity15, Identity16]
      (implicit 
        ev1: Schema[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)],
        ev2: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, Unit)))))))))))))))) <:< ColsRepr,
        ev3: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]], Features.Source[Identity10, Source]], Features.Source[Identity11, Source]], Features.Source[Identity12, Source]], Features.Source[Identity13, Source]], Features.Source[Identity14, Source]], Features.Source[Identity15, Source]], Features.Source[Identity16, Source]],
        ev4: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14 with Identity15 with Identity16
      ): SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16), ColsRepr, AllColumnIdentities, Source] =
        new SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16), ColsRepr, AllColumnIdentities, Source] {}

    implicit def tuple17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14, Identity15, Identity16, Identity17]
      (implicit 
        ev1: Schema[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)],
        ev2: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, Unit))))))))))))))))) <:< ColsRepr,
        ev3: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]], Features.Source[Identity10, Source]], Features.Source[Identity11, Source]], Features.Source[Identity12, Source]], Features.Source[Identity13, Source]], Features.Source[Identity14, Source]], Features.Source[Identity15, Source]], Features.Source[Identity16, Source]], Features.Source[Identity17, Source]],
        ev4: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14 with Identity15 with Identity16 with Identity17
      ): SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17), ColsRepr, AllColumnIdentities, Source] =
        new SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17), ColsRepr, AllColumnIdentities, Source] {}

    implicit def tuple18[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14, Identity15, Identity16, Identity17, Identity18]
      (implicit
        ev1: Schema[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)],
        ev2: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, Unit)))))))))))))))))) <:< ColsRepr,
        ev3: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]], Features.Source[Identity10, Source]], Features.Source[Identity11, Source]], Features.Source[Identity12, Source]], Features.Source[Identity13, Source]], Features.Source[Identity14, Source]], Features.Source[Identity15, Source]], Features.Source[Identity16, Source]], Features.Source[Identity17, Source]], Features.Source[Identity18, Source]],
        ev4: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14 with Identity15 with Identity16 with Identity17 with Identity18
      ): SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18), ColsRepr, AllColumnIdentities, Source] =
        new SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18), ColsRepr, AllColumnIdentities, Source] {}

    implicit def tuple19[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14, Identity15, Identity16, Identity17, Identity18, Identity19]
      (implicit 
        ev1: Schema[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)],
        ev2: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, Unit))))))))))))))))))) <:< ColsRepr,
        ev3: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]], Features.Source[Identity10, Source]], Features.Source[Identity11, Source]], Features.Source[Identity12, Source]], Features.Source[Identity13, Source]], Features.Source[Identity14, Source]], Features.Source[Identity15, Source]], Features.Source[Identity16, Source]], Features.Source[Identity17, Source]], Features.Source[Identity18, Source]], Features.Source[Identity19, Source]],
        ev4: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14 with Identity15 with Identity16 with Identity17 with Identity18 with Identity19
      ): SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19), ColsRepr, AllColumnIdentities, Source] =
        new SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19), ColsRepr, AllColumnIdentities, Source] {}
    
    implicit def tuple20[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14, Identity15, Identity16, Identity17, Identity18, Identity19, Identity20]
      (implicit
        ev1: Schema[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)],
        ev2: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, Unit)))))))))))))))))))) <:< ColsRepr,
        ev3: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]], Features.Source[Identity10, Source]], Features.Source[Identity11, Source]], Features.Source[Identity12, Source]], Features.Source[Identity13, Source]], Features.Source[Identity14, Source]], Features.Source[Identity15, Source]], Features.Source[Identity16, Source]], Features.Source[Identity17, Source]], Features.Source[Identity18, Source]], Features.Source[Identity19, Source]], Features.Source[Identity20, Source]],
        ev4: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14 with Identity15 with Identity16 with Identity17 with Identity18 with Identity19 with Identity20
      ): SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20), ColsRepr, AllColumnIdentities, Source] =
        new SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20), ColsRepr, AllColumnIdentities, Source] {}

    implicit def tuple21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14, Identity15, Identity16, Identity17, Identity18, Identity19, Identity20, Identity21]
      (implicit 
        ev1: Schema[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)],
        ev2: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, Unit))))))))))))))))))))) <:< ColsRepr,
        ev3: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]], Features.Source[Identity10, Source]], Features.Source[Identity11, Source]], Features.Source[Identity12, Source]], Features.Source[Identity13, Source]], Features.Source[Identity14, Source]], Features.Source[Identity15, Source]], Features.Source[Identity16, Source]], Features.Source[Identity17, Source]], Features.Source[Identity18, Source]], Features.Source[Identity19, Source]], Features.Source[Identity20, Source]], Features.Source[Identity21, Source]],
        ev4: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14 with Identity15 with Identity16 with Identity17 with Identity18 with Identity19 with Identity20 with Identity21
      ): SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21), ColsRepr, AllColumnIdentities, Source] =
        new SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21), ColsRepr, AllColumnIdentities, Source] {}

    implicit def tuple22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14, Identity15, Identity16, Identity17, Identity18, Identity19, Identity20, Identity21, Identity22]
      (implicit
        ev1: Schema[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)],
        ev2: (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, Unit)))))))))))))))))))))) <:< ColsRepr,
        ev3: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]], Features.Source[Identity10, Source]], Features.Source[Identity11, Source]], Features.Source[Identity12, Source]], Features.Source[Identity13, Source]], Features.Source[Identity14, Source]], Features.Source[Identity15, Source]], Features.Source[Identity16, Source]], Features.Source[Identity17, Source]], Features.Source[Identity18, Source]], Features.Source[Identity19, Source]], Features.Source[Identity20, Source]], Features.Source[Identity21, Source]], Features.Source[Identity22, Source]],
        ev4: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14 with Identity15 with Identity16 with Identity17 with Identity18 with Identity19 with Identity20 with Identity21 with Identity22
      ): SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22), ColsRepr, AllColumnIdentities, Source] =
        new SchemaValidity[F, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22), ColsRepr, AllColumnIdentities, Source] {}
  }
  // format: on

    // format: off
  trait SchemaValidityCaseClasses {
    implicit def caseClass1[F, A, Z, ColsRepr, AllColumnIdentities, Source, Identity1, S <: Singleton with String](implicit
      ccSchema: Schema.CaseClass1[A, Z],
      ev: ColsRepr <:< (A, Unit),
      ev2: F <:< Features.Source[Identity1, Source],
      ev3: AllColumnIdentities =:= Identity1
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] {}

    implicit def caseClass2[S1 <: Singleton with String,       
        S2 <: Singleton with String, F, A1, A2, Z, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2](implicit
      ccSchema: Schema.CaseClass2[A1, A2, Z],
      ev: ColsRepr <:< (A1, (A2, Unit)),
      ev2: F <:<  :||:[Features.Source[Identity1, Source], Features.Source[Identity2, Source]],
      ev3: AllColumnIdentities =:= Identity1 with Identity2
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] {}

    implicit def caseClass3[F, A1, A2, A3, Z, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3](implicit
      ccSchema: Schema.CaseClass3[A1, A2, A3, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, Unit))),
     ev2: F <:< Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]],
     ev3: AllColumnIdentities =:= Identity1 with Identity2 with Identity3
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] {}

    implicit def caseClass4[F, A1, A2, A3, A4, Z, ColsRepr,  AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4](implicit
      ccSchema: Schema.CaseClass4[A1, A2, A3, A4, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, Unit)))),
     ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],
     ev3: AllColumnIdentities =:= Identity1 with Identity2 with Identity3 with Identity4
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] {}

    implicit def caseClass5[F, A1, A2, A3, A4, A5, Z, ColsRepr,  AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5](implicit
      ccSchema: Schema.CaseClass5[A1, A2, A3, A4, A5, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, Unit))))),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]],  Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] {}

    implicit def caseClass6[F, A1, A2, A3, A4, A5, A6, Z, ColsRepr,  AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6](implicit
      ccSchema: Schema.CaseClass6[A1, A2, A3, A4, A5, A6, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, Unit)))))),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] {}

    implicit def caseClass7[F, A1, A2, A3, A4, A5, A6, A7, Z, ColsRepr,  AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7](implicit
      ccSchema: Schema.CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, Unit))))))),
     ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]],
     ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] {}

    implicit def caseClass8[F, A1, A2, A3, A4, A5, A6, A7, A8, Z, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8](implicit
      ccSchema: Schema.CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, Unit)))))))),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] {}

    implicit def caseClass9[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, Z, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9](implicit
      ccSchema: Schema.CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, Unit))))))))),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] {}

    implicit def caseClass10[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10](implicit
      ccSchema: Schema.CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, Unit)))))))))),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]], Features.Source[Identity10, Source]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] {}

    implicit def caseClass11[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11](implicit
      ccSchema: Schema.CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, Unit))))))))))),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]], Features.Source[Identity10, Source]], Features.Source[Identity11, Source]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] {}

    implicit def caseClass12[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12](implicit
      ccSchema: Schema.CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, Unit)))))))))))),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]], Features.Source[Identity10, Source]], Features.Source[Identity11, Source]], Features.Source[Identity12, Source]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] {}

    implicit def caseClass13[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13](implicit
      ccSchema: Schema.CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, Unit))))))))))))),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]], Features.Source[Identity10, Source]], Features.Source[Identity11, Source]], Features.Source[Identity12, Source]], Features.Source[Identity13, Source]], 
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] {}

    implicit def caseClass14[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14](implicit
      ccSchema: Schema.CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, Unit)))))))))))))),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]], Features.Source[Identity10, Source]], Features.Source[Identity11, Source]], Features.Source[Identity12, Source]], Features.Source[Identity13, Source]], Features.Source[Identity14, Source]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] {}

    implicit def caseClass15[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14, Identity15](implicit
      ccSchema: Schema.CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, Unit))))))))))))))),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]], Features.Source[Identity10, Source]], Features.Source[Identity11, Source]], Features.Source[Identity12, Source]], Features.Source[Identity13, Source]], Features.Source[Identity14, Source]], Features.Source[Identity15, Source]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14 with Identity15
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] {}

    implicit def caseClass16[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14, Identity15, Identity16](
      implicit
      ccSchema: Schema.CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, Unit)))))))))))))))),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]], Features.Source[Identity10, Source]], Features.Source[Identity11, Source]], Features.Source[Identity12, Source]], Features.Source[Identity13, Source]], Features.Source[Identity14, Source]], Features.Source[Identity15, Source]], Features.Source[Identity16, Source]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14 with Identity15 with Identity16
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] {}

    implicit def caseClass17[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14, Identity15, Identity16, Identity17](
      implicit
      ccSchema: Schema.CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, Unit))))))))))))))))),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]], Features.Source[Identity10, Source]], Features.Source[Identity11, Source]], Features.Source[Identity12, Source]], Features.Source[Identity13, Source]], Features.Source[Identity14, Source]], Features.Source[Identity15, Source]], Features.Source[Identity16, Source]], Features.Source[Identity17, Source]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14 with Identity15 with Identity16 with Identity17
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] {}

    implicit def caseClass18[
      F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14, Identity15, Identity16, Identity17, Identity18
    ](implicit
      ccSchema: Schema.CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, Unit)))))))))))))))))),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]], Features.Source[Identity10, Source]], Features.Source[Identity11, Source]], Features.Source[Identity12, Source]], Features.Source[Identity13, Source]], Features.Source[Identity14, Source]], Features.Source[Identity15, Source]], Features.Source[Identity16, Source]], Features.Source[Identity17, Source]], Features.Source[Identity18, Source]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14 with Identity15 with Identity16 with Identity17 with Identity18
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] {}

    implicit def caseClass19[
      F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14, Identity15, Identity16, Identity17, Identity18, Identity19
    ](implicit
      ccSchema: Schema.CaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z],
      ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, Unit))))))))))))))))))),
      ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]], Features.Source[Identity10, Source]], Features.Source[Identity11, Source]], Features.Source[Identity12, Source]], Features.Source[Identity13, Source]], Features.Source[Identity14, Source]], Features.Source[Identity15, Source]], Features.Source[Identity16, Source]], Features.Source[Identity17, Source]], Features.Source[Identity18, Source]], Features.Source[Identity19, Source]],
      ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14 with Identity15 with Identity16 with Identity17 with Identity18 with Identity19
    ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] =
      new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] {}

    // TODO fix scalafmt error - it fails in caseClass20 and above because of - `Search state exploded`
    // https://scalameta.org/scalafmt/docs/known-issues.html#deeply-nested-code

    // implicit def caseClass20[
    //    F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14, Identity15, Identity16, Identity17, Identity18, Identity19, Identity20
    // ](implicit
    //   ccSchema: Schema.CaseClass20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z],
    //   ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, Unit)))))))))))))))))))),
    //   ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]], Features.Source[Identity10, Source]], Features.Source[Identity11, Source]], Features.Source[Identity12, Source]], Features.Source[Identity13, Source]], Features.Source[Identity14, Source]], Features.Source[Identity15, Source]], Features.Source[Identity16, Source]], Features.Source[Identity17, Source]], Features.Source[Identity18, Source]], Features.Source[Identity19, Source]], Features.Source[Identity20, Source]],
    //   ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14 with Identity15 with Identity16 with Identity17 with Identity18 with Identity19 with Identity20
    // ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] =
    //   new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] {}

    // implicit def caseClass21[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14, Identity15, Identity16, Identity17, Identity18, Identity19, Identity20, Identity21
    // ](implicit
    //   ccSchema: Schema.CaseClass21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z],
    //   ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, Unit))))))))))))))))))))),
    //   ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]], Features.Source[Identity10, Source]], Features.Source[Identity11, Source]], Features.Source[Identity12, Source]], Features.Source[Identity13, Source]], Features.Source[Identity14, Source]], Features.Source[Identity15, Source]], Features.Source[Identity16, Source]], Features.Source[Identity17, Source]], Features.Source[Identity18, Source]], Features.Source[Identity19, Source]], Features.Source[Identity20, Source]], Features.Source[Identity21, Source]], 
    //   ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14 with Identity15 with Identity16 with Identity17 with Identity18 with Identity19 with Identity20 with Identity21
    // ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] =
    //   new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] {}

    // implicit def caseClass22[F, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z, ColsRepr, AllColumnIdentities, Source, Identity1, Identity2, Identity3, Identity4, Identity5, Identity6, Identity7, Identity8, Identity9, Identity10, Identity11, Identity12, Identity13, Identity14, Identity15, Identity16, Identity17, Identity18, Identity19, Identity20, Identity21, Identity22
    // ](implicit
    //   ccSchema: Schema.CaseClass22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z],
    //   ev: ColsRepr <:< (A1, (A2, (A3, (A4, (A5, (A6, (A7, (A8, (A9, (A10, (A11, (A12, (A13, (A14, (A15, (A16, (A17, (A18, (A19, (A20, (A21, (A22, Unit)))))))))))))))))))))),
    //   ev2: F <:< Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]], Features.Source[Identity3, Source]], Features.Source[Identity4, Source]],  Features.Source[Identity5, Source]], Features.Source[Identity6, Source]], Features.Source[Identity7, Source]], Features.Source[Identity8, Source]], Features.Source[Identity9, Source]], Features.Source[Identity10, Source]], Features.Source[Identity11, Source]], Features.Source[Identity12, Source]], Features.Source[Identity13, Source]], Features.Source[Identity14, Source]], Features.Source[Identity15, Source]], Features.Source[Identity16, Source]], Features.Source[Identity17, Source]], Features.Source[Identity18, Source]], Features.Source[Identity19, Source]], Features.Source[Identity20, Source]], Features.Source[Identity21, Source]], Features.Source[Identity22, Source]],
    //   ev3: AllColumnIdentities <:< Identity1 with Identity2 with Identity3 with Identity4 with Identity5 with Identity6 with Identity7 with Identity8 with Identity9 with Identity10 with Identity11 with Identity12 with Identity13 with Identity14 with Identity15 with Identity16 with Identity17 with Identity18 with Identity19 with Identity20 with Identity21 with Identity22
    // ): SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] =
    //   new SchemaValidity[F, Z, ColsRepr, AllColumnIdentities, Source] {}
  }
  // format: on
}
