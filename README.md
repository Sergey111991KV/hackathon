# Description

## Реализованная функциональность

API для взаимодействия с приложением.
Генерация статических и динамических токенов.
Геймификация проведения покупок и поощрительные призы.

Сервер написан на Haskell, использовался в архитектуре использовался AppleHanlde патерн. 
Основные настройки кастомизируемы, подключено логирование и использование ОРМ.
Использовал Docker & PostGIS для базы данных.
Библиотеки по Haskell в файле package.yaml

Необходимо сделать свой файл config.conf по шаблону с template.conf, в нем указать все необходимые настройки.

Команды по запуску 
    
    - docker-compose up -d db

Для запуска самого сервера необходимо скачать stack и использовать комманду

    - stack build

    - stack ghci

Для запуска миграции запустить команду

    - runAllMigrations

В дальнейшем предпологается использовать CI с автоматическим развертыванием и отладкой.

# API

## USER

### GET user/get/:id -> JSON UserSerializer

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

### POST user/save  JSON UserCreation -> ()

Стандартные эндпоинты создания и получения пользователя (UserSerializer)

## Plaid-Token

### Get plaid-token/get JSON PlaidTokenGet  -> JSON PlaidToken

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

###  Get token/exchange JSON ChangePlaidToken -> JSON UserToken

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

###  Post token/deactivate JSON UserToken -> ()

Этот эндпоинт для владельцев бизнеса и организаторов. Отправка UserToken на него делает этот токен больше не пригодным для списания.


## All-User-Information

### Get /user/all-information/:id  -> JSON AllInformation

Этот эндпоинт выводит информацию о пользователе, о всех его транзакциях и всех мероприятиях(Events) или подписках(Subsriptions) на которых он был когда либо. Включает в себя призы и подарки.

example:

    Request:

    {
    "success": true,
    "result": {
        "userEvents": [
            {
                "idEvents": 1,
                "endDate": "2021-06-22",
                "reatedAt": "2021-05-22T14:20:51.48677Z",
                "userId": 1
            }
        ],
        "user": {
            "phone": "34567890123",
            "createdAt": "2021-05-22T14:17:45.820602Z",
            "age": 22,
            "bill": 800,
            "bonusBill": 810,
            "nativeCity": "Novorossiysk",
            "isOrganization": null,
            "userId": 1,
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
        },
        "transaction": [
            {
                "amount": 200,
                "createdAt": "2021-05-22T14:20:51.491828Z",
                "fromId": 1,
                "toId": 1,
                "userId": 1,
                "transactionToIdType": "Events"
            }
        ],
        "userSubsriptions": []
        }
    }

## Events

Это эндпоинты по создание и получению мероприятий

### events/getOne/:id  -> Events

### events/save JSON EventsCreation -> ()

### events/getAll -> JSON [Events]

example:

    Request:

    {
    "creationName" : "Финальный матч",
    "creationType" : "Спортивные мероприятия",
    "creationUrl" : "novorossiys/futboll-club",
    "creationDateEventsStart" : "2021-05-22",
    "creationDateEventsEnd" : "2021-06-22",
    "creationPrizeFirstType" : "Билет на Финал на Городском стадионе",
    "creationPrizeFirstCategories" : 1,
    "creationPrizeSecondType" : "Футболки с подпсисями команды",
    "creationPrizeSecondCategories" : 5,
    "creationPrizeTrirdType" : "Футбольный мяч",
    "creationPrizeTrirdCategories" : 30,
    "creationPrice" : 200
    }
 
    Response: 

    {
    "typeE": "Спортивные мероприятия",
    "prizeTrirdCategoriesE": 30,
    "dateEventsStartE": "2021-05-22",
    "prizeSecondCategoriesE": 5,
    "nameE": "Финальный матч",
    "createdE": "2021-05-22T14:37:20.655198Z",
    "prizeFirstCategoriesE": 1,
    "prizeTrirdTypeE": "Футбольный мяч",
    "priceE": 200,
    "prizeFirstTypeE": "Билет на Финал на Городском стадионе",
    "urlE": "novorossiys/futboll-club",
    "dateEventsEndE": "2021-06-22",
    "prizeSecondTypeE": "Футболки с подпсисями команды"
    }