module Data.Deployment (Deployment(..)) where

-- | The kind of deployment used. The standard is used, e.g., when running
-- | locally or in Kubernetes. In the future `Salesforce` will be added.
data Deployment
  = Standard
