using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace SampleLinq
{
    class Program
    {
        static void Main(string[] args)
        {
            // Tutorial based on https://www.youtube.com/watch?v=gwD9awr3NNo&list=PLGLfVvz_LVvRX6xK1oi0reKci6ignjdSa&index=15
            // Language Integrated Query (LINQ) provides many tools for working with data.
            // LINQ is similar to SQL, but can work with data aside from databases.
            // You manipulate data using Query Expressions.
            Console.WriteLine("Sample LINQ application\n");

            // Queries
            QueryStringArray();
            int[] intArray = QueryIntArray();
            Console.WriteLine("Returned array...note that the data is from the original query");
            foreach (int i in intArray)
            {
               Console.WriteLine("  {0}", i);
            }
            Console.WriteLine();

            QueryArrayList();
            QueryCollection();
            QueryAnimalData();
            Console.ReadLine();
        }

        static void QueryStringArray()
        {
            // Given a list of dog names, get strings with spaces and put them into alphabetical order
            Console.WriteLine("### Query a string array ###");
            string[] dogs = {
                "K 9", "Brian Griffin", "Scooby Doo", "Old Yeller", "Rin Tin Tin",
                "Benji", "Charlie B. Barkin", "Lassie", "Snoopy"};
 
            // "from" states where data comes from and where to store each piece as you cycle 
            // "where" defines conditions that must be met
            // "orderby" defines what data is used for ordering results (ascending / descending)
            // "select" defines the variable that is checked against the condition
            var dogSpaces =
                from dog in dogs
                where dog.Contains(" ")
                orderby dog descending
                select dog;
            foreach (var doggy in dogSpaces)    // "doggy" can also be of type "string"
            {
                Console.WriteLine("  {0}", doggy);
            }
            Console.WriteLine();
        }

        static int[] QueryIntArray()
        {
            // Given an array of numbers, get integers bigger than 20 and order them
            Console.WriteLine("### Query an integer array ###");
            int[] nums = { 5, 10, 15, 20, 25, 30, 35 };
 
            // Get numbers bigger than 20
            var gt20 =
                from num in nums
                where num > 20    // Or "where (num > 20)" or "where (num > 20) && (num <= 30)"
                orderby num
                select num;
            foreach (var i in gt20)
            {
                Console.WriteLine($"  {i}");
            }
            Console.WriteLine();
 
            // The type is an enumerable
            Console.WriteLine($"Get type for LINQ quest : {gt20.GetType()}");

            // You can convert it into a list or array
            var listGT20 = gt20.ToList<int>();
            var arrayGT20 = gt20.ToArray();

            // Changing the data automatically updates the query
            Console.WriteLine("Change the data, which automatically updates the query");
            nums[0] = 40;
            foreach (var i in gt20)
            {
               Console.WriteLine("  {0}", i);
            }
            Console.WriteLine();

            // Return the array...note thisis from the initial query (not the updated query)
            // because we converted the query into an array before updating the data.
            return arrayGT20;
        }

        static void QueryArrayList()
        {
            // Given a list of famous animals (actually characters!), return smaller animals (limited by weight)
            Console.WriteLine("### Query a generic ArrayList ###");
            ArrayList famAnimals = new ArrayList()
            {
                new Animal { Name = "Heidi", Height = 60.5, Weight = 18.2 },
                new Animal { Name = "Shrek", Height = 210.0, Weight = 130.5 },
                new Animal { Name = "Congo", Height = 280.7, Weight = 90.0 }
            };
 
            // Convert the ArrayList into an IEnumerable
            var famAnimalEnum = famAnimals.OfType<Animal>();
            var smallAnimals =
                from animal in famAnimalEnum
                where animal.Weight <= 90.0
                orderby animal.Name
                select animal;
 
            foreach (var animal in smallAnimals)
            {
                Console.WriteLine($"  {animal.ToString()}");
            }
            Console.WriteLine();
        }

        static void QueryCollection()
        {
            // Given a collection of dog breeds, return large dogs (limited by weight and height)
            Console.WriteLine("### Query a List<> collection ###");
            var dogsList = new List<Animal>()
            {
                new Animal { Name = "German Shepherd", Height = 62.5, Weight = 33.3 },
                new Animal { Name = "Chihuahua", Height = 17.5, Weight = 1.9 },
                new Animal { Name = "Saint Bernard", Height = 75.0, Weight = 86.4 }
            };
 
            var bigDogs =
                from dog in dogsList
                where (dog.Weight > 30.0) && (dog.Height > 62.5)
                orderby dog.Name
                select dog;
 
            foreach (var dog in bigDogs)
            {
                Console.WriteLine($"  {dog.ToString()}");
            }
            Console.WriteLine();
        }

        static void QueryAnimalData()
        {
            Console.WriteLine("### Perform joins on animal and owner data ###");
            Animal[] animals = new[]
            {
                new Animal { Name = "German Shepherd", Height = 62.5, Weight =  33.3, OwnerID = 1 },
                new Animal { Name = "Chihuahua", Height = 17.5, Weight = 1.9, OwnerID = 2 },
                new Animal { Name = "Saint Bernard", Height = 75.0, Weight = 86.4, OwnerID = 3 },
                new Animal { Name = "Pug", Height = 30.0, Weight = 6.9, OwnerID = 1 },
                new Animal { Name = "Beagle", Height = 37.5, Weight = 9.9, OwnerID = 2 }
            };
 
            Owner[] owners = new[]
            {
                new Owner { Name = "Doug Parks", OwnerID = 1 },
                new Owner { Name = "Sally Smith", OwnerID = 2 },
                new Owner { Name = "Paul Brooks", OwnerID = 3 }
            };
 
            // Create a new list of animals with just name and height
            // Note: You could include "animal.OwnerID" to include the ID of the its' owner
            var listNameHeight =
                from animal in animals
                select new
                {
                    animal.Name,
                    animal.Height
                };
 
            // Convert to an object array
            Array arrNameHeight = listNameHeight.ToArray();
            Console.WriteLine("(animals listed by Name and Height)");
            foreach (var i in arrNameHeight)
            {
                Console.WriteLine($"  {i.ToString()}");
            }
            Console.WriteLine();
 
            // Create an inner join
            // * Join info in owners and animals using equal values for IDs
            // * Store values for animal and owner
            Console.WriteLine("# Inner join #");
            var innerJoin =
               from animal in animals
               join owner in owners on animal.OwnerID
               equals owner.OwnerID
               select new
               {
                   // These are placeholder names (they can be anything, like "zzz")
                   OwnerName = owner.Name,
                   OwnerID = owner.OwnerID,
                   AnimalName = animal.Name
               };

            foreach (var i in innerJoin)
            {
                Console.WriteLine("  {0} (ID {1}) owns a {2} ",
                    i.OwnerName, i.OwnerID, i.AnimalName);
            }
 
            Console.WriteLine();
 
            // Create a group inner join
            // * For each owner, get all animals and put them into a group if their ID matches the owner ID
            Console.WriteLine("# Group join #");
            var groupJoin =
                from owner in owners
                orderby owner.OwnerID
                join animal in animals on owner.OwnerID
                equals animal.OwnerID into ownerGroup
                select new
                {
                    ThisOwner = owner.Name,
                    OwnedAnimals =
                        from owner2 in ownerGroup
                        orderby owner2.Name
                        select owner2
                };
 
            int totalAnimals = 0;
            int spaces = 0;
            foreach (var ownerGroup in groupJoin)
            {
                Console.WriteLine($"  {ownerGroup.ThisOwner}");
                foreach (var animal in ownerGroup.OwnedAnimals)
                {
                    totalAnimals++;
                    spaces = (20-animal.Name.Length);
                    Console.WriteLine($"    * {{0}}{{1,{spaces}}}", animal.Name, totalAnimals);
                }
            }
        }
    }
}
