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
toggleTodoState' (Todo todo) = case todo.todoState of
                                    Active -> Todo (todo{todoState = Completed})
                                    Completed -> Todo (todo{todoState = Active})

todoUpdateTitle :: String -> Todo -> Todo
todoUpdateTitle title (Todo todo) = Todo (todo { todoTitle = title })

-- | toggleTodoState
--
toggleTodoState :: TodoState -> TodoState
toggleTodoState Active    = Completed
toggleTodoState Completed = Active

-- | defaultTodo
--
defaultTodo :: String -> Todo
defaultTodo s = Todo { todoId: 0, todoTitle: s, todoState: Active }

-- | defaultTodoId
--
defaultTodoId :: Int -> String -> Todo
defaultTodoId todo_id s = Todo { todoId: todo_id, todoTitle: s, todoState: Active }
