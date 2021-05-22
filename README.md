# API

## USER

### GET user/get/:id -> UserSerializer

example:

    Response: 
        {
            "phone": "34567890123",
            "createdAt": "2021-05-22T03:56:21.886635Z",
            "age": 22,
            "bill": 1000,
            "bonusBill": 800,
            "nativeCity": "Novorossiysk",
            "isOrganization": true,
            "userId": 6,
            "achievements": [
                "Travel",
                "CityActivities"
            ],
            "secondName": "Ivanov",
            "firstName": "Igor",
            "interests": [
                "Food",
                "Art"
                ]
        }

### POST user/save  JSON UserCreation ()

Стандартные эндпоинты создания и получения пользователя (UserSerializer)

## Plaid-Token

### Get plaid-token/get JSON PlaidTokenGet  -> PlaidToken

Получение Токена или генерация токена для оплаты. Для его получения мы отправляем объект с данными о подключаемой(покупаемой) услуги: Events (какие либо события, временные) и Subscriptions (подписки на проезд). QR код получается может быть статическим, но мы на сервере будем проверять его актуальность, точнее токена, который генерируется для QR кода.

example:

    Request:

        {
            "typePay" : "Events",
            "idPayAction" : 1,
            "amount" : 200
        }
 
    Response: 
    
        {
            "success": true,
            "result": "a3ab665fafc4e0446f30c3efb524358480bbef5b93730edd410b4418e477"
        }



## User-Token

###  Get token/exchange JSON ChangePlaidToken -> UserToken

Здесь мы на основании токена-оплаты получаем токен-подтверждение от пользователя, который является подтверждением списания денег со счета пользователя и покупки той или иной услуги или же просто подтверждает о наличии купленной услуги. Он будет иметь свой срок действия, который потом вынесем в конфиги

example:

    Request:

        {
            "payToken" : "a3ab665fafc4e0446f30c3efb524358480bbef5b93730edd410b4418e477",
            "userIdChange" :  6
        }
 
    Response: 

        {
            "userId": 6,
            "userToken": "eedc540e18add23bd5b951f36cf52e87c8138c77fff6b710a5aa3457f736"
        }

###  Post token/deactivate JSON UserToken ()

Этот эндпоинт для владельцев бизнеса и организаторов. Отправка UserToken на него делает этот токен больше не пригодным для списания.




