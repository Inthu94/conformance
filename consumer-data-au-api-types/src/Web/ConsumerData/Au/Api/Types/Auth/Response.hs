{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeOperators             #-}

module Web.ConsumerData.Au.Api.Types.Auth.Response where
import Web.ConsumerData.Au.Api.Types.Auth.Common

-- | Response data returned to the client's server after the customer has successfully auth'd. This data is sent via the user's user-agent, using a redirected (302) HTTP GET to the client's @redirect_uri@.
data AuthorisationResponse =
  AuthorisationResponse
  { -- authorisation-code, id_token, state
--TODO


  }
  | AuthorisationResponseError
  { -- TODO: Need to confirm what other fields are returned for an error
      _error             :: GrantErrorResponseType  -- Required
    , _error_description :: Maybe ErrorDescription  -- Optional
    , _error_uri         :: Maybe AuthUri          -- Optional
    , _state             :: Maybe State             -- Required if a "state" parameter was present in the client authorization request.  The exact value received from the client.
    -- TODO: does "state" a required parameter in the authorization request?
  }
  -- ^ Error response for issuing an access token <https://tools.ietf.org/html/rfc6749#section-4.1.2.1 OAuth 2.0 §4.1.2.1> or implicit grant error response <https://tools.ietf.org/html/rfc6749#section-4.2.2.1 OAuth 2.0 §4.2.2.1>


-- | Response data returned to the client, after it HTTP POSTs the @authorization-code@ to the /token endpoint on the authorisation server.
data TokenResponse =
  TokenResponseSuccess TokenResponseR
  | TokenResponseError TokenResponseErrorR

data TokenResponseR =
  TokenResponseR
  { -- TODO: need confirmation from the oauth specs for these fields
    -- TODO: Some fields are only Maybe because this could be an error; need to change this type
      _tokenResponseAccessToken  :: Maybe AccessToken
    , _tokenResponseTokenType    :: Maybe TokenTypeDescription
    , _tokenResponseExpiresIn    :: Maybe TokenMaxAgeSeconds
    , _tokenResponseScope        :: Maybe Scopes
    , _tokenResponseRefreshToken :: Maybe RefreshToken
    , _tokenResponseIdToken      :: Maybe (IdToken 'TokenUse)
  }

data TokenResponseErrorR =
  TokenResponseErrorR
  {
  -- TODO: Need to confirm what other fields are returned for an error
    _tokenResponseError            :: TokenErrorResponseType  -- Required
  , _tokenResponseErrorDescription :: Maybe ErrorDescription  -- Optional
  , _tokenResponseErrorUri         :: Maybe AuthUri          -- Optional
  }
  -- ^ Error response for issuing an access token <https://tools.ietf.org/html/rfc6749#section-5.2 OAuth 2.0 §5.2>
