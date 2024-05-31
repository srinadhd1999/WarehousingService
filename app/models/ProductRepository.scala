package models

import javax.inject.{Inject, Singleton}
import play.api.db.slick.DatabaseConfigProvider
import slick.jdbc.JdbcProfile
import scala.concurrent.{ExecutionContext, Future}

case class Product(id: String, name: String, quantity: Int, user: String, location: String)

@Singleton
class ProductRepository @Inject()(dbConfigProvider: DatabaseConfigProvider)(implicit ec: ExecutionContext) {
  val dbConfig = dbConfigProvider.get[JdbcProfile]

  import dbConfig._
  import profile.api._

  class ProductTable(tag: Tag) extends Table[Product](tag, "products") {
    def id = column[String]("id", O.PrimaryKey)
    def name = column[String]("name")
    def quantity = column[Int]("quantity")
    def user = column[String]("user")
    def location = column[String]("location")
    def * = (id, name, quantity, user, location) <> ((Product.apply _).tupled, Product.unapply)
  }

  val products = TableQuery[ProductTable]

  def list(): Future[Seq[Product]] = db.run(products.result)

  def find(location: String): Future[Option[Product]] = db.run(products.filter(_.location === location).result.headOption)

  def add(id: String, name: String, quantity: Int, user: String, location: String): Future[Product] = db.run {
    println("In add product method in repository")
    (products += Product(id, name, quantity, user, location)).map(_ => (Product(id, name, quantity, user, location)))
  }

  def update(product: Product): Future[Int] = db.run(products.filter(_.id === product.id).update(product))

  def incrementQuantity(id: String, location: String, increment: Int): Future[Int] = {
    val query = for { product <- products if product.id === id && product.location === location} yield product.quantity
    val updateAction = query.result.headOption.flatMap {
      case Some(currentQuantity) =>
        val newQuantity = currentQuantity + increment
        query.update(newQuantity)
      case None =>
        DBIO.successful(0) // No rows affected
    }
    db.run(updateAction.transactionally)
  }

  def decrementQuantity(id: String, location: String, increment: Int): Future[Int] = {
    val query = for { product <- products if product.id === id && product.location === location} yield product.quantity
    val updateAction = query.result.headOption.flatMap {
      case Some(currentQuantity) =>
        val newQuantity = currentQuantity - increment
        query.update(newQuantity)
      case None =>
        DBIO.successful(0) // No rows affected
    }
    db.run(updateAction.transactionally)
  }

  def fetchProducts(user: String): Future[Seq[Product]] = db.run(products.filter(_.user === user).result)

  def delete(id: String, location: String): Future[Int] = db.run {
    products.filter(product => product.id === id && product.location === location).delete
  }

  def lessStockFilter(): Future[Seq[Product]] = db.run(products.filter(_.quantity <=3 ).result)
}
