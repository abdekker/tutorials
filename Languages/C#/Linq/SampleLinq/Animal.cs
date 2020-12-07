namespace SampleLinq
{
    class Animal
    {
        public string Name { get; set; }
        public double Weight { get; set; }
        public double Height { get; set; }
        public int OwnerID { get; set; }
 
        public Animal(
            string name = "No Name",
            double weight = 0.0,
            double height = 0.0)
        {
            Name = name;
            Weight = weight;
            Height = height;
        }
 
        public override string ToString()
        {
            return string.Format("{0} weighs {1:0.0}kg and is {2:0.0}cm tall",
                Name, Weight, Height);
        }
    }
}
