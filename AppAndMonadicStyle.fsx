type CustomerId = CustomerId of int
type EmailAddress = EmailAddress of string
type CustomerInfo = {
    id: CustomerId
    email: EmailAddress
    }

type Result<'a> =
    | Success of 'a
    | Failure of string list
// list of strings not string

let createCustomerId id =
    if id > 0 then
        Success (CustomerId id)
    else
        Failure ["CustomerId must be positive"]

let createEmailAddress str =
    if System.String.IsNullOrEmpty(str) then
        Failure ["Email must not be empty"]
    elif str.Contains("@") then
        Success (EmailAddress str)
    else
        Failure ["Email must contain @-sign"]

module Result =

    let map f xResult =
        match xResult with
        | Success x ->
            Success (f x)
        | Failure errs ->
            Failure errs

    // "return" is a keyword in F#, so abbreviate it
    let retn x =
        Success x

    let apply fResult xResult =
        match fResult,xResult with
        | Success f, Success x ->
            Success (f x)
        | Failure errs, Success x ->
            Failure errs
        | Success f, Failure errs ->
            Failure errs
        | Failure errs1, Failure errs2 ->
            // concat both lists of errors
            Failure (List.concat [errs1; errs2])

    let bind f xResult =
        match xResult with
        | Success x ->
            f x
        | Failure errs ->
            Failure errs

//APPLICATIVE STYLE
let createCustomer customerId email =
    { id=customerId;  email=email }

let (<!>) = Result.map
let (<*>) = Result.apply

// applicative version
let createCustomerResultA id email =
    let idResult = createCustomerId id
    let emailResult = createEmailAddress email
    createCustomer <!> idResult <*> emailResult

let goodId = 1
let badId = 0
let goodEmail = "test@example.com"
let badEmail = "example.com"

let goodCustomerA =
    createCustomerResultA goodId goodEmail

let badCustomerA =
    createCustomerResultA badId badEmail

// MONADIC STYLE

let (>>=) x f = Result.bind f x
// monadic version
let createCustomerResultM id email =
    createCustomerId id >>= (fun customerId ->
    createEmailAddress email >>= (fun emailAddress ->
    let customer = createCustomer customerId emailAddress
    Success customer
    ))

let goodCustomerM =
    createCustomerResultM goodId goodEmail

let badCustomerM =
    createCustomerResultM badId badEmail

