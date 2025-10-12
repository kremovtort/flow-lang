-- | Main AST module - aggregator for all surface-level AST components
--
-- This module provides a unified interface to the Flow language's surface AST,
-- re-exporting types from the modularized Surface/ submodules.
--
-- Migration note: Old monolithic definitions are preserved as type synonyms
-- for backward compatibility during transition.

module Flow.AST (
  -- Core modules
  module Flow.AST.Common,
  module Flow.AST.Type,
  module Flow.AST.Ann,

  -- Surface AST modules
  module Flow.AST.Surface.Module,
  module Flow.AST.Surface.Decl,
  module Flow.AST.Surface.Callable,
  module Flow.AST.Surface.Callable.Defs,
  module Flow.AST.Surface.Syntax,
  module Flow.AST.Surface.Pattern,
) where

import Flow.AST.Common
import Flow.AST.Type
import Flow.AST.Ann
import Flow.AST.Surface.Module
import Flow.AST.Surface.Decl
import Flow.AST.Surface.Callable
import Flow.AST.Surface.Callable.Defs
import Flow.AST.Surface.Syntax
import Flow.AST.Surface.Pattern
