local tolerance = 10
function isturnback (angle)
   angle = angle % 360
   return (math.abs(angle - 180) < tolerance)
end

function isturnbackrad (angle)
   angle = angle % (2*math.pi)
   return (math.abs(angle - math.pi) < tolerance)
end
