# haskell-mnist

## 使い方

### stack.yaml

```yaml
packages:
- .
- location:
    git: https://github.com/lvs7k/haskell-mnist.git
    commit: 5f7efdd4c5364b6b39de76a0b0609113f750e563
  extra-dep: true
```

### Main.hs

```haskell
import Data.Mnist (Mnist(..), loadMnist, toOneHotLabel)
import Numeric.LinearAlgebra

main :: IO ()
main = do
    m <- loadMnist True -- normalize
    print $ testLabel m
```

## 参考
[https://github.com/oreilly-japan/deep-learning-from-scratch/blob/master/dataset/mnist.py](https://github.com/oreilly-japan/deep-learning-from-scratch/blob/master/dataset/mnist.py)
