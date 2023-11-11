# Level 1 - What is your dream?
## Introduction
"**Welcome to the Motoko Academy - we are thrilled to have you here!**" As you stand at the entrance, a large luminous hologram greets you.

Behind the glow of the hologram, the large hall opens up, filled with energy and innovation. You first notice the screens filled with lines of code - it's not just  display, you can see live feeds of ongoing projects, showcasing the real-time development of groundbreaking applications and systems.

There are also monitors around, displaying images of innovative projects and groundbreaking technology. 

On one screen, you notice a short-movie about a  DAO focused on preserving biodiversity. Members from around the world collaborate to fund and manage conservation projects, using blockchain to ensure transparency and equitable participation.

On another screen, you can see a city, but it looks different than what you might expect. The buildings have plants growing all over them, bringing a splash of green to the usual gray of the city. You can see people walking around, but they are not in cars. Instead, they are using different types of electric vehicles, from bikes to scooters to skateboards. You learn that the city is self-sufficient, producing its own food and energy. Also, the city is run by a DAO, where all the citizens are members. 

In one corner of the room, there’s a huge fish tank. But this fish tank is a bit different. You noyice little robots swimming around with the fish. The robots are there to check if the water is clean and to help take care of the plants. It's like gardeners but for the underwater world.

This hall isn't just a typical classroom. It’s clear that this is a place of growth, discovery, and connection to something greater than oneself. 

While you're lost in your thought, the artifical voice from the hologram gently asks: "**What is your dream?**"

This is about turning your dreams into reality. That why before you dive into Motoko and coding, it's time to grow and develop your vision. 
## 🎯 Mission
Your task, should you decide to embark on this journey, is to lay down the foundation and set the direction for your DAO. Here at Motoko Academy, we believe that a strong foundation is key to creating a successful DAO, and it all starts with a clear and well-defined dream. <br/>
So, take this moment to reflect: 
- How do you want to live your life?
- What’s your idea (doesn't matter if it's small or big)?
- What do you want to achieve? 
- What's your vision for the future?

Your DAO will be the vessel to help you achieve your dreams.

### Task 1: Define a name for you DAO 
Implement an immutable variable `name` of type `Text` that represents the name of your DAO.

### Task 2: Define a manifesto for you DAO 
Implement a mutable variable `manifesto` of type `Text` that represents the manifesto of your DAO.

_A manifesto is a public declaration of the intentions, motives, or views of an individual or group. It is often political in nature, but may present an individual's life stance. [Source - Wikipedia](https://en.wikipedia.org/wiki/Manifesto)_

### Task 3: Implement the `getName` query function
The `getName` function should:
- Takes no parameters
- Return the name of your DAO
``` motoko
getName : shared query () -> async Text;
```

### Task 4: Implement the `getManifesto` query function
The `getManifesto` function should:
- Takes no parameters
- Return the manifesto of your DAO
``` motoko
getManifesto : shared query () -> async Text;
```

### Task 5: Implement the `setManifesto` update function
The `setManifesto` function should:
- Take a `manifesto` as a parameter of type `Text`
- Set the `manifesto` variable to the value of the parameter
- Return nothing
``` motoko
setManifesto : shared (manifesto : Text) -> async ();
```

### Task 6: Define a list of goals for your DAO
Implement a mutable variable `goals` of type `Buffer<Text>` that represents the goals of your DAO.

### Task 7: Implement the `addGoal` function
The `addGoal` function should:
- Take a `goal` as a parameter of type `Text`
- Add a new `goal` to the `goals` buffer
- Return nothing
```motoko
addGoal : shared (goal : Text) -> async ();
```

### Task 8: Implement the `getGoals` query function
The `getGoals` function should:
- Take no parameters
- Return all the `goals` of your DAO in an `Array`
```motoko
getGoals : shared query () -> async [Text];
```

## 📺 Interface
At the end of this level, your canister should implement the following interface:

```motoko
actor {
    getName : shared query () -> async Text;
    getManifesto : shared query () -> async Text;
    setManifesto : shared (manifesto : Text) -> async ();
    addGoal : shared (goal : Text) -> async ();
    getGoals : shared query () -> async [Text];
}
```
## 📚 Resources
| Name | Type | URL | Description |
| ---- | ---- | --- | ----------- |
| Fundamental concepts | Lesson | [Chapter 1](https://github.com/motoko-bootcamp/dao-adventure/blob/main/lessons/chapter-1/CHAPTER-1.MD) | Learn about the fundamental concepts of Motoko - a must read! |
| Common programming concepts  | Lesson | [Chapter 2](https://github.com/motoko-bootcamp/dao-adventure/blob/main/lessons/chapter-2/CHAPTER-2.MD) | Learn about variables, functions and basic expressions in Motoko. |
| Primitive Types  | Lesson | [Chapter 3](https://github.com/motoko-bootcamp/dao-adventure/blob/main/lessons/chapter-3/CHAPTER-3.MD) | Learn about primitive types in Motoko. |
| Candid | Lesson | [Chapter 4](https://github.com/motoko-bootcamp/dao-adventure/blob/main/lessons/chapter-4/CHAPTER-4.MD) | Learn about Candid the IDL of the Internet Computer and why it matters |
| Buffer  | Documentation | [Base Library - Buffer](https://internetcomputer.org/docs/current/motoko/main/base/Buffer) | The official documentation for the `Buffer` library in Motoko |
| Unlocking the power of DAOs | Appendix | [Appendix](https://github.com/motoko-bootcamp/dao-adventure/blob/main/appendix/appendix-1/APPENDIX-1.MD) | Learn about the power of DAOs |