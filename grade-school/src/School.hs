module School (School, add, empty, grade, sorted) where

import Data.List

type Name = String
type Grade = Int
data Student = Student Grade Name 
    deriving (Eq, Ord)

newtype School = School [Student]

getGrade :: Student -> Grade
getGrade (Student g _) = g

getStudents :: School -> [Student]
getStudents (School s) = s

add :: Int -> String -> School -> School
add gradeNum student school = School (s:ss)
    where
        s = Student gradeNum student
        ss = getStudents school

empty :: School
empty = School []

grade :: Int -> School -> [String]
grade gradeNum = names. getFromGrade . getStudents
    where
        getFromGrade = filter correctGrade
        correctGrade (Student g _) = g == gradeNum
        names = foldl (\acc (Student g n) -> n:acc) []

sorted :: School -> [(Int, [String])]
sorted = map addName . groupBy (\x y -> getGrade x == getGrade y) . sort . getStudents

addName :: [Student] -> (Grade, [Name])
addName (Student g n:students) = sortNames unsorted
    where
        appendName (g, xs) (Student _ a) = (g, a:xs)
        unsorted = foldl appendName (g, [n]) students
        sortNames (g, names) = (g, reverse names)
