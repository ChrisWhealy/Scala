object supermarket {
  // --------------------------------------------------------------------------
  // Various Constants
  val UNKNOWN_SKU_TXT = "Unknown SKU"
  val UNKNOWN_SKU     = new Item(UNKNOWN_SKU_TXT, 0)
  val NO_DISCOUNT     = new Discount(1,0)
  val GBP             = "£"
  val EUR             = "€"
  val USD             = "$"
  val DOT             = "."
  val COMMA           = ","

  // --------------------------------------------------------------------------
  // Change the currency code and decimal separator to values appropriate for
  // your locale
  val CURRENCY_SYMBOL   = GBP
  val DECIMAL_SEPARATOR = DOT

  val DISCOUNT_TXT_LEN  = 22

  // --------------------------------------------------------------------------
  // Data types
  // --------------------------------------------------------------------------

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Item class
  class Item(pDesc: String, pUnitPrice: Int) {
    var desc      = pDesc
    var unitPrice = pUnitPrice
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Discount class
  class Discount(pQty: Int, pAmount: Int) {
    var qty    = pQty
    var amount = pAmount

    override def toString: String =
      if (amount > 0)
        " (" + formatPrice(amount, CURRENCY_SYMBOL) +
        " off for buying " + qty + ")"
      else
        " "
  }

  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Bill item class
  class BillItem(pQty: Int, pItem: Item, pDiscount: Discount) {
    var qty      = pQty
    var item     = pItem
    var discount = pDiscount

    def price =
      BillItem.calcPrice(this.qty)(this.item.unitPrice)(this.discount)
  }

  // --------------------------------------------------------------------------
  // BillItem companion object
  object BillItem {
    // ------------------------------------------------------------------------
    // Pricing functions
    def calcPrice(qty: Int)(unitPrice: Int)(discount: Discount): Int = {
      def disc = calcDiscount(qty)(discount.qty)(discount.amount)
      calcAmount(qty)(unitPrice)(disc)
    }

    private def calcDiscount(itemQty: Int)(discountQty: Int)(discountAmt: Int) =
      Math.floor(itemQty / discountQty).toInt * discountAmt

    private def calcAmount(itemQty: Int)(unitPrice: Int)(discount: Int) =
      (itemQty * unitPrice) - discount
  }

  // --------------------------------------------------------------------------
  // Items in stock
  // All prices are stored in pennies until the bill is printed.
  //  Only at that point is the decimal formatting applied
  val stockList = Map(
        56 -> new Item("Onions (pack of 4)", 83)
    ,   57 -> new Item("Tomatoes (1kg)", 199)
    ,  347 -> new Item("Butter (250g)", 152)
    ,  554 -> new Item("Beef Mince (500g)", 199)
    ,  643 -> new Item("Potatoes (2.5kg)", 198)
    ,  670 -> new Item("Apples (pack of 5)", 194)
    , 1019 -> new Item("Plain Yoghurt (500g)",	94)
    , 1094 -> new Item("Lettuce", 49)
    , 1234 -> new Item("Dry Sherry, 1lt", 1010)
    , 1373 -> new Item("Chicken Breast (pack of 2)", 300)
    , 2275 -> new Item("Rice (1kg)",	148)
    , 3111 -> new Item("Pizza (350g)", 260)
    , 3263 -> new Item("Cola (2L)", 99)
    , 3317 -> new Item("Oranges (pack of 5)", 150)
    , 4705 -> new Item("Fresh Orange Juice (1L)", 169)
    , 4719 -> new Item("Fish Fingers (500g)", 335)
    , 5079 -> new Item("Curry Sauce (500g jar)", 184)
    , 5243 -> new Item("Bananas (1kg)", 268)
    , 5643 -> new Item("Soap Powder", 500)
    , 5671 -> new Item("Red wine (70cl bottle)",750)
    , 5712 -> new Item("Pineapple", 259)
    , 5907 -> new Item("Bread (800g loaf)", 145)
    , 6034 -> new Item("Pasta (1kg)", 120)
    , 6386 -> new Item("Cheese (250g)", 174)
    , 6469 -> new Item("Beer (pack of 4 bottles)", 400)
    , 6487 -> new Item("Milk (2.27L / 4 pints)", 219)
    , 6564 -> new Item("Salmon Fillets (pack of 2)", 323)
    , 7102 -> new Item("Pasta Sauce (500g jar)", 184)
    , 8903 -> new Item("Mango", 75)
    , 9190 -> new Item("Milk (568ml / 1 pint)",	68)
  )

  def longestDesc = stockList.values.maxBy(_.desc.length).desc.length + 1

  // --------------------------------------------------------------------------
  // The current shopping basket
  val basket: List[Int] = List(670,3317,643,1234,5907,6034,670,1234,5671,7102,7102,1094,1373,7102,5671,6469)

  // --------------------------------------------------------------------------
  // What items are discounted?
  val discountedSkus = Map(
      1234 -> new Discount(2,250)
    , 5671 -> new Discount(2,125)
    , 7102 -> new Discount(3,50)
  )

  // --------------------------------------------------------------------------
  // Get an item from a generic map object
  def genericMapGet[SomeObj](someMap: Map[Int,SomeObj])
                            (notThereValue: SomeObj)
                            (theThing: Int) =
    someMap.get(theThing) match {
      case Some(theThing) => Some(theThing).get
      case None           => notThereValue
    }

  // --------------------------------------------------------------------------
  // Read SKU from generic stock list
  var readItem = genericMapGet(stockList)(UNKNOWN_SKU)(_: Int)

  // Fetch the discount for a given SKU (which might be zero)
  var readDiscount = genericMapGet(discountedSkus)(NO_DISCOUNT)(_: Int)

  // --------------------------------------------------------------------------
  // Increment the quantity for an existing item on the bill
  def updateQty(bill: collection.mutable.Map[Int,BillItem])(sku: Int) =
    (bill.get(sku) match {
      case Some(sku) => Some(sku).get.qty
      case None      => 0
    }) + 1

  // --------------------------------------------------------------------------
  // Add an item in the shopping basket to the bill.
  // Discounts amounts cannot be calculated until after all the items in the
  // shopping basket have been added to the bill
  def addItemToBill(acc: collection.mutable.Map[Int,BillItem], sku: Int): collection.mutable.Map[Int,BillItem] = {
    // First, update the billed quantity for this item and read both the sku and
    // discount objects.  Pass all of these as arguments to the inner function
    val newQty       = updateQty(acc)(sku)
    val thisItem     = readItem(sku)
    val thisDiscount = readDiscount(sku)

    // If the item is an unknown SKU, then ignore it by simply returning the
    // accumulator, else add/update the item to/in the accumulator
    if (thisItem.desc == UNKNOWN_SKU_TXT)
      acc
    else
      acc += (sku -> new BillItem(newQty, thisItem, thisDiscount))
  }

  // --------------------------------------------------------------------------
  // Formatting functions
  def formatPennies(price: Int) = {
    val pennies = price % 100

    if (pennies == 0)
      "00"
    else if (pennies < 10)
      "0" + pennies
    else
      pennies
  }

  def formatPounds(price: Int) = Math.floor(price / 100).toInt

  def formatPrice(price: Int, currSymbol: String) = currSymbol + formatPounds(price) + DECIMAL_SEPARATOR + formatPennies(price)

  def formatDesc(desc: String, max: Int) = desc.padTo(max," ").mkString

  def formatQty(qty: Int) = (if (qty < 10) " " else "") + qty

  // --------------------------------------------------------------------------
  // Create the shopping bill
  val shoppingBill = basket.foldLeft(collection.mutable.Map[Int,BillItem]())(addItemToBill)


  // --------------------------------------------------------------------------
  // Print stock list
  stockList.values.foreach { i =>
    print(formatDesc(i.desc, longestDesc))
    print(if (i.unitPrice < 1000) " " else "")
    println(formatPrice(i.unitPrice, CURRENCY_SYMBOL))
  }

  // --------------------------------------------------------------------------
  // Print the shopping bill
  def printItem(acc: Int, item: BillItem) = {
    val discQty = Math.floor(item.qty / item.discount.qty).toInt

    print(formatQty(item.qty) + " ")
    print(formatDesc(item.item.desc + item.discount.toString, longestDesc + DISCOUNT_TXT_LEN))
    print(if (item.price < 1000) " " else "")
    println(formatPrice(item.price, CURRENCY_SYMBOL))

    acc + item.price
  }

  // --------------------------------------------------------------------------
  // Print the total line at the end of the shopping bill
  val total = shoppingBill.values.toList.foldLeft(0)(printItem)

  println((" " * (longestDesc + DISCOUNT_TXT_LEN - 4)) +
          "Total: " + formatPrice(total, CURRENCY_SYMBOL))
}