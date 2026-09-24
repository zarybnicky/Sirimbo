const errorMessages: Record<string, string> = {
  INVALID_CREDENTIALS: 'Nesprávné jméno nebo heslo',
  INVITATION_NOT_FOUND: 'Pozvánka není platná',
  INVITATION_ALREADY_USED: 'Pozvánka již byla použita',
  INVALID_EMAIL: 'Zadejte e-mail',
  'duplicate key value violates unique constraint "users_email_key"':
    'Zřejmě již v systému máte účet. Přihlaste se prosím.',
};

export function getErrorMessage(error: string) {
  return errorMessages[error];
}
