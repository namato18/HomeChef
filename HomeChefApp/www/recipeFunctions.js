  // Save recipe to local storage
  Shiny.addCustomMessageHandler("saveRecipe", function(message) {
    let name = message.name;
    let ingredients = message.ingredients;
    let instructions = message.instructions;
    
    console.log('starting save');
    let recipe = { name: name, ingredients: ingredients, instructions: instructions };
    let recipes = JSON.parse(localStorage.getItem('recipes')) || [];
    recipes.push(recipe);
    localStorage.setItem('recipes', JSON.stringify(recipes));
    console.log('recipe saved');
  })
  
  // Retrieve recipes from local storage
  Shiny.addCustomMessageHandler("getRecipes", function(message) {
    console.log(message.message);
    var response = JSON.parse(localStorage.getItem('recipes')) || [];
    Shiny.setInputValue('jsResponse', response);
  })
  