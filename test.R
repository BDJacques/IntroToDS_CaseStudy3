t = read.csv(file = "./Video_Game_Sales_by_Gregory_Smith/vgsales.csv")

test1 = which(t$Platform == "Wii")
test2 = which(t$Genre == "Sports")
test3 = which(t$Publisher == "Nintendo")

#This comparison tests if two which lists intersected are equivalent to a full which statement.
testA = intersect(test1, test2)
compareA = which(t$Platform == "Wii" & t$Genre == "Sports")

compareDT = data.frame(intersect = testA, which = compareA)

#This does the same but with three lists and two intersects

testB = intersect(test1, test2)
testC = intersect(testB, test3)
compareB = which(t$Platform == "Wii" & t$Genre == "Sports" & t$Publisher == "Nintendo")

compare3DT = data.frame(intersect = testC, which = compareB)