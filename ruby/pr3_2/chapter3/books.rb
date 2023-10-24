class BookInStock
  attr_reader :isbn
  attr_accessor :price

  def initialize isbn, price
    @isbn = isbn
    @price = Float price
  end

  def price_in_cents
    (price * 100).round
  end

  def price_in_cents= cents
    @price = cents / 100.0
  end

  def to_s
    "ISBN: #{@isbn}, price: #{@price}"
  end
end
