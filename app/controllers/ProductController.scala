package controllers

import models.{ProductRepository, UserRepository, Product, User}
import play.api.libs.json._
import play.api.mvc._
import services.KafkaMessageProducer
import java.util.UUID
import javax.inject._
import scala.concurrent.{ExecutionContext, Future}

class ProductController @Inject()(productRepository: ProductRepository, userRepository: UserRepository,
                                  kafkaMessageProducer: KafkaMessageProducer, cc: ControllerComponents)(implicit ec: ExecutionContext) extends AbstractController(cc) {
  implicit val userFormat: Format[Product] = Json.format[Product]

  def getList: Action[AnyContent] = Action.async {
    productRepository.list().map { product =>
      Ok(Json.toJson(product))
    }
  }

  private def idGenerator(): String = {
    val uuid = UUID.randomUUID()
    val shortId = uuid.toString.take(8)  // Take the first 8 characters
    shortId
  }

  def addProduct(): Action[AnyContent] = Action.async { implicit request =>
    val postVals = request.body.asFormUrlEncoded
    postVals.map { args =>
      val id = args("product_id").head
      val name = args("product_name").head
      val quantity = args("quantity").head.toInt
      val location = args("location").head
      val user = userRepository.getUserFromLocation(location)
      val uId = idGenerator()
      user.flatMap { userName =>
        productRepository.add(id, name, quantity, userName, location)
        val jsonString = s"{\"id\":\"$uId\", \"operation\":\"add_new\", \"add_quantity\":\"$quantity\", \"product_name\":\"$name\", \"product_id\":\"$id\", \"location\": \"$location\"}"
        kafkaMessageProducer.sendMessage(jsonString)
      }
      productRepository.list().map { products =>
        Redirect(routes.ProductController.listProducts("admin"))
      }
    }.getOrElse(Future.successful(Redirect(routes.ProductController.listProducts("admin"))))
  }

  def listProducts(user: String): Action[AnyContent] = Action.async {
    if (user != "admin") {
      productRepository.fetchProducts(user).map { products =>
        Ok(views.html.userView(products))
      }
    } else {
      productRepository.list().map { products =>
        Ok(views.html.adminView(products))
      }
    }
  }

  def deleteProduct(): Action[AnyContent] = Action.async { implicit request =>
    val postVals = request.body.asFormUrlEncoded
    postVals.map { args =>
      val id = args("product_id").head
      val location = args("location").head
      productRepository.delete(id, location)
      val uId = idGenerator()
      val jsonString = s"{\"id\":\"$uId\", \"operation\":\"delete_current\", \"product_id\":\"$id\", \"location\": \"$location\"}"
      kafkaMessageProducer.sendMessage(jsonString)
      productRepository.list().map { products =>
        Redirect(routes.ProductController.listProducts("admin"))
      }
    }.getOrElse(Future.successful(Redirect(routes.ProductController.listProducts("admin"))))
  }

  def updateProductQuantity(): Action[AnyContent] = Action.async { implicit request =>
    val postVals = request.body.asFormUrlEncoded
    var name = ""
    postVals.map { args =>
      val id = args("product_id").head
      val user = args("user").head
      val quantityToIncrease = args("quantity").head.toInt
      val location = args("location").head
      name = user
      val uId = idGenerator()
      productRepository.incrementQuantity(id, location, quantityToIncrease)
      val jsonString = s"{\"id\":\"$uId\", \"operation\":\"increase_current\", \"increase_content\":\"$quantityToIncrease\", \"product_id\":\"$id\", \"location\": \"$location\"}"
      kafkaMessageProducer.sendMessage(jsonString)
      productRepository.list().map { products =>
        Redirect(routes.ProductController.listProducts(user))
      }
    }.getOrElse(Future.successful(Redirect(routes.ProductController.listProducts(name))))
  }

  def reduceProductQuantity(): Action[AnyContent] = Action.async { implicit request =>
    val postVals = request.body.asFormUrlEncoded
    var name = ""
    postVals.map { args =>
      val id = args("product_id").head
      val userName = args("user").head
      val quantityToDecrease = args("quantity").head.toInt
      val location = args("location").head
      val uId = idGenerator()
      productRepository.decrementQuantity(id, location, quantityToDecrease)
      val jsonString = s"{\"id\":\"$uId\", \"operation\":\"decrease_current\", \"decrease_content\":\"$quantityToDecrease\", \"product_id\":\"$id\", \"location\": \"$location\"}"
      kafkaMessageProducer.sendMessage(jsonString)
      productRepository.list().map { products =>
        Redirect(routes.ProductController.listProducts(userName))
      }
    }.getOrElse(Future.successful(Redirect(routes.ProductController.listProducts(name))))
  }

  def lessStockViewer(): Action[AnyContent] = Action.async {
    productRepository.lessStockFilter().map { products =>
      Ok(views.html.lessStock(products))
    }
  }

  def logOutAction: Action[AnyContent] = Action {
    Redirect(routes.LoginController.index)
  }
}
