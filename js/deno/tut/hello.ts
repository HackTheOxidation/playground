import Person, { sayHello } from "./person.ts";

const ada: Person = {
  firstName: "Ada",
  lastName: "Lovelace",
};

console.log(sayHello(ada));
