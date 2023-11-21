-- criando struct
data Funcionario = Funcionario{
    nomeFunc :: String,
    salarioFunc :: Double,
    idadeFunc :: Int
} deriving(Show)

-- calculo do beneficio
calculoBeneficio :: Funcionario -> Double
calculoBeneficio funcionario = salarioFunc funcionario * 0.1 --10% nesse caso

-- calculo do imposto
calculoImposto :: Funcionario -> Double
calculoImposto funcionario = salarioFunc funcionario * 0.2 -- 20% do salario como imposto

-- processando uma lista de funcionarios
processaFunc :: [Funcionario] -> [(String, Double, Double)]
processaFunc = map(\func -> (nomeFunc func, calculoBeneficio func, calculoImposto func))

-- le os dados dos funcionarios
leFuncionariosDoArquivo :: FilePath -> IO [Funcionario]
leFuncionariosDoArquivo caminhoArquivo = do
    conteudo <- readFile caminhoArquivo
    return $ map parseFuncionario (lines conteudo)

-- cada linha do arq = 1 funcionario
parseFuncionario :: String -> Funcionario
parseFuncionario linha =
    let [nome, salario, idade] = words (map (\c -> if c == ',' then ' ' else c) linha)
    in Funcionario nome (read salario) (read idade)

-- escreve o resultado no arquivo
escreveResultado :: FilePath -> [(String, Double, Double)] -> IO ()
escreveResultado caminhoArquivo resultados =
    writeFile caminhoArquivo (unlines (map (\(nome, beneficio, imposto) -> nome ++ ": Beneficio: " ++ show beneficio ++ ", Imposto: " ++ show imposto) resultados))

-- main
main :: IO ()
main = do
    funcionarios <- leFuncionariosDoArquivo "funcionarios.txt"
    let resultados = processaFunc funcionarios
    escreveResultado "resultados.txt" resultados