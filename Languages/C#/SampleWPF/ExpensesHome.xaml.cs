using System.Windows;
using System.Windows.Controls;

namespace SampleWPF
{
    /// <summary>
    /// Interaction logic for ExpensesHome.xaml
    /// </summary>
    public partial class ExpensesHome : Page
    {
        public ExpensesHome()
        {
            InitializeComponent();
        }

        private void btnViewExpenses_Click(object sender, RoutedEventArgs e)
        {
            // View Expense Report
            ExpensesReportPage expenseReportPage = new ExpensesReportPage(this.peopleListBox.SelectedItem);
            this.NavigationService.Navigate(expenseReportPage);
        }
    }
}
