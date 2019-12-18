
type Animal = 
    | Dog of string
    | Cat of string 
    | Horse of string
    | Hamster of string

type DogRace = Jamnik | Bokser | Blablador
type Dog = Dog of string * DogRace

type CatRace = Dachowiec | Domowy
type Cat = Cat of string * CatRace

type HorseRace = Arab | Rosyjski 
type Horse = Horse of string * HorseRace

type Vehicle =  Car of string * string

let Burek =  Dog("Burek", DogRace.Blablador)
let Łajka = Dog("Łajka", DogRace.Jamnik)
let Mruczek = Cat("Mruczek", CatRace.Dachowiec)
let Bonifacy = Cat("Bonifacy", CatRace.Domowy)
let Dangerous = Horse("Dangerous", HorseRace.Arab)
let Bucefał = Horse("Bucefał", HorseRace.Arab)
let Supra = Car("Toyota", "Supra")
let Challenger = Car("Dodge", "Challenger")



let Doggies = [Burek, Łajka]
let Kitties = [Bonifacy, Mruczek]
let Horsies = [Bucefał , Dangerous]

let Cars = [Supra, Challenger]


let MaybeDog = Some Burek
let MaybeCat = Some Mruczek
let MaybeHorse = Some Bucefał

let MaybeCar = Some Supra