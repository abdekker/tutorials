using System.Collections.ObjectModel;
using System.Windows;
using System.Windows.Controls;

namespace SampleWPF
{
    /// <summary>
    /// Interaction logic for ExpensesHome.xaml
    /// </summary>
    public partial class ExpensesHome : Page
    {
        private ObservableCollection<TreeViewItem> TreeFamily { get; set; }

        public ExpensesHome()
        {
            TreeFamily = new ObservableCollection<TreeViewItem>();
            TreeFamily.Add(GetLoadedTreeRoot());
            InitializeComponent();
        }

        private TreeViewItem GetLoadedTreeRoot()
        {
            TreeViewItem parent = new TreeViewItem() { Header = "Parent" };
            TreeViewItem child1 = new TreeViewItem() { Header = "Child 1" };
            TreeViewItem child2 = new TreeViewItem() { Header = "Child 2" };
            TreeViewItem grandchild1 = new TreeViewItem() { Header = "Grandchild 1" };
            TreeViewItem grandchild2 = new TreeViewItem() { Header = "Grandchild 2" };

            child1.Items.Add(grandchild1);
            child2.Items.Add(grandchild2);
            parent.Items.Add(child1);
            parent.Items.Add(child2);
            return parent;
        }

        private void btnViewExpenses_Click(object sender, RoutedEventArgs e)
        {
            // View Expense Report
            ExpensesReportPage expenseReportPage = new ExpensesReportPage(this.peopleListBox.SelectedItem);
            this.NavigationService.Navigate(expenseReportPage);
        }
    }
}
