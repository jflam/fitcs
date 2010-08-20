using System;
using System.IO;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace Fit.Parser.Tests {
    [TestClass]
    public class Tests {
        public Tests() {
        }

        private TestContext testContextInstance;

        /// <summary>
        ///Gets or sets the test context which provides
        ///information about and functionality for the current test run.
        ///</summary>
        public TestContext TestContext {
            get {
                return testContextInstance;
            }
            set {
                testContextInstance = value;
            }
        }

        #region Additional test attributes
        //
        // You can use the following additional attributes as you write your tests:
        //
        // Use ClassInitialize to run code before running the first test in the class
        // [ClassInitialize()]
        // public static void MyClassInitialize(TestContext testContext) { }
        //
        // Use ClassCleanup to run code after all tests in a class have run
        // [ClassCleanup()]
        // public static void MyClassCleanup() { }
        //
        // Use TestInitialize to run code before running each test 
        // [TestInitialize()]
        // public void MyTestInitialize() { }
        //
        // Use TestCleanup to run code after each test has run
        // [TestCleanup()]
        // public void MyTestCleanup() { }
        //
        #endregion

        [TestMethod]
        public void ReactToIntegers() {
            IObservable<int> source = Observable.Range(5, 10);
            

            IDisposable subscription = source.Subscribe(
                x => Console.WriteLine("Received {0} from source", x),
                ex => Console.WriteLine("Source signaled an error: {0}", ex.Message),
                () => Console.WriteLine("Source said there are no messages to follow anymore")
            );
        }

        [TestMethod]
        public void MLParser1() {
            MiniMLParserFromString parser = new MiniMLParserFromString();
            Result<string, Term> result =
                parser.All(@"let true = \x.\y.x in 
                         let false = \x.\y.y in 
                         let if = \b.\l.\r.(b l) r in
                         if true then false else true;");
        }

        [TestMethod]
        public void MLParser2() {
            MiniMLParserFromStream parser = new MiniMLParserFromStream();
            Result<Stream, Term> result = parser.All(File.OpenRead(@"c:\git\fit\Fit.Parser.Tests\test.ml"));
        }

        [TestMethod]
        public void FitParser1() {
            FitParserFromStream parser = new FitParserFromStream();
            var obj = parser.Header(File.OpenRead(@"c:\git\fit\Fit.Parser.Tests\sample.fit"));
            Assert.IsNotNull(obj);
            Header header = obj.Value;
            Assert.AreEqual(header.Length, 12);
            Assert.AreEqual(header.ProtocolVersion, 0x10);
            Assert.AreEqual(header.ProfileVersion, 0x40);
        }
    }
}
