using NUnit.Framework;
using System;
using System.Collections;
using System.IO;
using System.Linq;
using System.Reflection;

namespace FsLatte.Tests.Data
{
    public static class TestDataSets
    {
        private static Lazy<IEnumerable> _goodFiles = new Lazy<IEnumerable>(GetGoodFiles);
        public static IEnumerable GoodFiles => _goodFiles.Value;
        private static IEnumerable GetGoodFiles()
        {
            var assembly = Assembly.GetExecutingAssembly();
            var resources = assembly.GetManifestResourceNames();
            var goodFiles = resources
                .Where(x => x.Contains(".good.") && x.EndsWith(".lat") && !x.Contains("18"));
            var filesContent = goodFiles
                .Select(path => new
                {
                    Path = path,
                    Content = new Lazy<string>(() => 
                    {
                        using (var stream = assembly.GetManifestResourceStream(path))
                        using (var streamReader = new StreamReader(stream))
                            return streamReader.ReadToEnd();
                    }),
                    ExpectedOutput = new Lazy<string>(() =>
                    {
                        var expectedOutputPath = String.Join(".", path.Split('.').Reverse().Skip(1).Reverse().Concat(new[] { "output" }));
                        using (var stream = assembly.GetManifestResourceStream(expectedOutputPath))
                        using (var streamReader = new StreamReader(stream))
                            return streamReader.ReadToEnd();
                    })
                })
                .ToArray();

            return filesContent
                .Select(fc => new TestCaseData(new TestData(fc.Content.Value, fc.ExpectedOutput.Value, GetTestName(fc.Path))));
        }

        private static Lazy<IEnumerable> _syntaxErrorsFiles = new Lazy<IEnumerable>(GetSyntaxErrorsFiles);
        public static IEnumerable SyntaxErrorsFiles => _syntaxErrorsFiles.Value;
        private static IEnumerable GetSyntaxErrorsFiles()
        {
            var assembly = Assembly.GetExecutingAssembly();
            var resources = assembly.GetManifestResourceNames();
            var syntaxErrorsFiles = resources.Where(x => x.Contains(".syntaxErrors.") && x.EndsWith(".lat"));
            var filesContent = syntaxErrorsFiles
                .Select(path => new
                {
                    Path = path,
                    Content = new Lazy<string>(() =>
                    {
                        using (var stream = assembly.GetManifestResourceStream(path))
                        using (var streamReader = new StreamReader(stream))
                            return streamReader.ReadToEnd();
                    })
                })
                .ToArray();

            return filesContent
                .Select(fc => new TestCaseData(new TestData(fc.Content.Value, null, GetTestName2(fc.Path,fc.Content.Value))));
        }

        private static Lazy<IEnumerable> _errorsFiles = new Lazy<IEnumerable>(GetErrorsFiles);
        public static IEnumerable ErrorsFiles => _errorsFiles.Value;
        private static IEnumerable GetErrorsFiles()
        {
            var assembly = Assembly.GetExecutingAssembly();
            var resources = assembly.GetManifestResourceNames();
            var syntaxErrorsFiles = resources.Where(x => x.Contains(".rest.") && x.EndsWith(".lat"));
            var filesContent = syntaxErrorsFiles
                .Select(path => new
                {
                    Path = path,
                    Content = new Lazy<string>(() =>
                    {
                        using (var stream = assembly.GetManifestResourceStream(path))
                        using (var streamReader = new StreamReader(stream))
                            return streamReader.ReadToEnd();
                    })
                })
                .ToArray();

            return filesContent
                .Select(fc => new TestCaseData(new TestData(fc.Content.Value, null, GetTestName2(fc.Path, fc.Content.Value))));
        }

        private static string GetTestName(string path) => String.Join(".", path.Split('.').Reverse().Take(3).Reverse());
        private static string GetTestName2(string path,string content) =>
            GetTestName(path) +"\\"
            + new string(content.TakeWhile(c => c != '\n' && c != '\r').Skip(3).ToArray());
    }
}