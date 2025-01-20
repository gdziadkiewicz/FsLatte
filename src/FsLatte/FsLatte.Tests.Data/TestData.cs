namespace FsLatte.Tests.Data
{
    public class TestData
    {
        public string Content;
        public string ExpectedOutput;
        private readonly string _testName;

        public TestData(string content, string expectedOutput)
        {
            Content = content;
            ExpectedOutput = expectedOutput;
        }

        public TestData(string content, string expectedOutput, string testName) : this(content, expectedOutput)
        {
            _testName = testName;
        }

        public override string ToString() => _testName;
    }
}