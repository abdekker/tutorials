
using System.Windows.Controls;

namespace SampleWPF
{
    /// <summary>
    /// Interaction logic for ExpensesReportPage.xaml
    /// </summary>
    public partial class ExpensesReportPage : Page
    {
        public ExpensesReportPage()
        {
            InitializeComponent();
        }

        // Custom constructor to pass expense report data
        public ExpensesReportPage(object data) : this()
        {
            // Bind to expense report data
            this.DataContext = data;
        }
    }
}
