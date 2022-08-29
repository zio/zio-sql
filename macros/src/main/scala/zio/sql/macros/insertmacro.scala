package zio.sql.macros

import scala.reflect.macros.blackbox

sealed trait InsertLike[F, Z, ColsRepr, AllColumnIdentities, Source]



object InsertLike {

    final case class CanBeInserted[F, Z, ColsRepr, AllColumnIdentities, Source]() extends InsertLike[F, Z, ColsRepr, AllColumnIdentities, Source]

    implicit def createInsertLike[F, Z, ColsRepr, AllColumnIdentities, Source]: InsertLike[F, Z, ColsRepr, AllColumnIdentities, Source] = macro createInsertLikeImpl[F, Z, ColsRepr, AllColumnIdentities, Source]

    /**
      * Steps:
        1. check if Z is either a case class or a Tuple
        2. Create nested tuple out of Z decls and compare with ColsRepr
        3. Create Features.Uniont[F, F] out of AllColumnIdentities tuple and compare with F
      */

    /**
       
        "persons.TableType" is just a Person
                most of the validation is here
                (String, (Int, Unit)) must be coming from Person, 
                all except for Optional fields
        inserted values must of the same types and order as cols repr
      
      
      
      */
    def createInsertLikeImpl[
        F: c.WeakTypeTag, 
        Z: c.WeakTypeTag, 
        ColsRepr: c.WeakTypeTag, 
        AllColumnIdentities: c.WeakTypeTag, 
        Source: c.WeakTypeTag](c: blackbox.Context): c.Expr[InsertLike[F, Z, ColsRepr, AllColumnIdentities, Source]] = {
        import c.universe._ 

        val fTpe = weakTypeOf[F]
        val zTpe = weakTypeOf[Z]
        val colsReprTpe = weakTypeOf[ColsRepr]
        val aciTpe = weakTypeOf[AllColumnIdentities]
        val sourceTpe = weakTypeOf[Source]


        c.Expr[InsertLike[F, Z, ColsRepr, AllColumnIdentities, Source]](
            q"new zio.sql.macros.InsertLike.CanBeInserted[${q"$fTpe"}, ${q"$zTpe"}, ${q"$colsReprTpe"}, ${q"$aciTpe"}, ${q"$sourceTpe"}]()"
        )

    }

  /**
            insertInto(persons)(name, age).values(("Jaro", 31))
  */

    /**
       * Schema is only to get A1, A2
       * 
       * Person(name, age), A1 == String, A2 == Int 
       *  ColsRepr            == (String, (Int, Unit))
       *  F                   == Features.Union[Features.Source["name"], Features.Source["age", Person]]
       *  Z                   == (A1, A2)
       *  AllColumnIdentities == ("name", "age")
       *  Source              == "persons".TableType
       * 
        
       implicit def tuple2[F, A1, A2, ColsRepr,  AllColumnIdentities, Source, Identity1, Identity2](implicit
            ev1: Schema[(A1, A2)],
            ev2: (A1, (A2, Unit)) <:< ColsRepr,
            ev3: F <:< Features.Union[Features.Source[Identity1, Source], Features.Source[Identity2, Source]],
            ev4: AllColumnIdentities =:= (Identity1, Identity2)
        ): SchemaValidity[F, (A1, A2), ColsRepr, AllColumnIdentities, Source] =
            new SchemaValidity[F, (A1, A2), ColsRepr, AllColumnIdentities, Source] {}

       */ 

}