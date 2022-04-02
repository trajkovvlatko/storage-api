## Storage API in Haskell

### Setup locally

#### Edit .env file
```
cp sample.env .env
```
Edit the contents of .env with proper db credentials. The file is in .gitignore.

#### Generate JSON web key
Authentication generates tokens and requires a .jwk.sig file to be present in the project root.

(Tip: Use https://mkjwk.org or similar, to easily generate a JSON web key online)

#### Setup development database
```
./bin/setup-dev.sh
```

#### Run development server
```
./bin/dev.sh
```

#### Setup test database
```
./bin/setup-test.sh
```

#### Run tests
```
./bin/test.sh
```

### Endpoints

Auth
- login: `POST /login`
- register: `POST /register`

Rooms

- list all rooms: `GET /rooms`
- preview a room: `GET /rooms/:id`
- create a room: `POST /rooms"`
- update a room: `PATCH /rooms/:id`
- delete a room: `DELETE /rooms/:id`

Storage units

- list all storage units per room: `GET /storage_units`
- preview a storage unit: `GET /storage_units/:id`
- create a storage unit in a room: `POST /storage_units`
- update a storage unit: `PATCH /storage_units/:id`
- delete a storage unit: `DELETE /storage_units/:id`

Drawers

- list all drawers per storage unit: `GET /drawers`
- preview a drawer: `GET /drawers/:id`
- create a drawer in a storage unit: `POST /drawers`
- update a drawer: `PATCH /drawers/:id`
- delete a drawer: `DELETE /drawers/:id`

Colors

- list all colors: `GET /colors`
- preview a color: `GET /colors/:id`
- create a color: `POST /colors`
- update a color: `PATCH /colors/:id`
- delete a color: `DELETE /colors/:id`

Item types

- list all item types: `GET /item_types`
- preview a item types: `GET /item_types/:id`
- create a item types: `POST /item_types`
- update a item types: `PATCH /item_types/:id`
- delete a item types: `DELETE /item_types/:id`

Search

- find an item: `GET /search/basic/:term`
