const errorMessages: Record<string, string> = {
  INVALID_CREDENTIALS: 'Nesprávné jméno nebo heslo',
  INVITATION_NOT_FOUND: 'Pozvánka není platná',
  INVITATION_ALREADY_USED: 'Pozvánka již byla použita',
  INVALID_EMAIL: 'Zadejte e-mail',
  'duplicate key value violates unique constraint "users_email_key"':
    'Pro tento e-mail už zřejmě existuje účet. Zkuste se přihlásit.',
};

export function getErrorMessage(error: string) {
  return errorMessages[error];
}
