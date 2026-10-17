const errorMessages: Record<string, string> = {
  INVALID_CREDENTIALS: 'Nesprávné jméno nebo heslo',
  INVITATION_NOT_FOUND: 'Pozvánka není platná',
  INVITATION_ALREADY_USED: 'Pozvánka již byla použita',
  INVALID_EMAIL: 'Zadejte e-mail',
  INVALID_PERSON_DETAILS: 'Zkontrolujte osobní údaje.',
  CAPACITY_EXCEEDED: 'Kapacita akce je již naplněná.',
  NOT_ALLOWED: 'Přihlašování na tuto akci není dostupné.',
  LESSON_LIMIT_EXCEEDED: 'Požadovaný počet lekcí už není k dispozici.',
  'duplicate key value violates unique constraint "users_email_key"':
    'Pro tento e-mail už zřejmě existuje účet. Zkuste se přihlásit.',
};

export function getErrorMessage(error: string) {
  return errorMessages[error];
}
