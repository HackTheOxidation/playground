N = 8

function isplaceok (a, n, c)
  for i = 1, n - 1 do
    if (a[i] == c) or
      (a[i] - i == c - n) or
      (a[i] + i == c + n) then
      return false
    end
  end
  return true
end

