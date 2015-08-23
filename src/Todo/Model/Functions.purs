module Model.Functions (
  todoFromObj,
  toggleTodoState',
  todoUpdateTitle,
  toggleTodoState,
  defaultTodo,
  defaultTodoId
) where

import Model.Types

todoFromObj (Todo todo) = todo

toggleTodoState' :: Todo -> Todo
toggleTodoState' (Todo todo) = case todo._todoState of
                                    Active -> Todo (todo{_todoState = Completed})
                                    Completed -> Todo (todo{_todoState = Active})

todoUpdateTitle :: String -> Todo -> Todo
todoUpdateTitle title (Todo todo) = Todo (todo { _todoTitle = title })

-- | toggleTodoState
--
toggleTodoState :: TodoState -> TodoState
toggleTodoState Active    = Completed
toggleTodoState Completed = Active

-- | defaultTodo
--
defaultTodo :: String -> Todo
defaultTodo s = Todo { _todoId: 0, _todoTitle: s, _todoState: Active }

-- | defaultTodoId
--
defaultTodoId :: Int -> String -> Todo
defaultTodoId todo_id s = Todo { _todoId: todo_id, _todoTitle: s, _todoState: Active }
