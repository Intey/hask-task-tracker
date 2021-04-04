module Domain.InputBounds where
import           Domain.Models


data CreateIssue = CI
    { summary      :: String
    , description  :: Maybe String
    , assignee     :: Maybe (Key User)
    , reporter     :: Key User
    , linkedIssues :: [Key Issue]
    }

data NewUser = NU { username :: String, password :: String}
