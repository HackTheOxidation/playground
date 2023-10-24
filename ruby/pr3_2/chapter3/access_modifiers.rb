class MyClass
  # default is "public"
  def method1
    # This method is public
  end

  protected
  # subsequent methods will be "protected"
  def method2
    # This method is protected
  end

  private
  # subsequent methods will be "private"
  def method3
    # This method is private
  end

  public
  # subsequent methods will be "public"
  def method4
    # This method is public
  end
end

class MyOtherClass
  # default is "public"
  def method1
    # This method is public
  end

  def method2
    # This method is protected
  end

  def method3
    # This method is private
  end

  def method4
    # This method is public
  end

  public :method1, :method4
  protected :method2
  private :method3
end

class YetAnotherClass
  def method1
    # This method is public
  end

  protected def method2
    # This method is protected
  end

  private def method3
    # This method is private
  end

  public def method4
    # This method is public
  end
end
