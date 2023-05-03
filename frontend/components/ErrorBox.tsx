const errorTranslation: { [key: string]: string } = {
  ACCOUNT_NOT_FOUND: 'Zadaná kombinace jména a e-mailu neexistuje',
  INVALID_PASSWORD: 'Nesprávné heslo',
  ACCOUNT_DISABLED: 'Učet byl zablokován',
  ACCOUNT_NOT_CONFIRMED: 'Účet ještě nebyl potvrzen',
};

type Props = {
  error: any;
  default?: string;
};

export const ErrorBox = ({ error: e, default: def }: Props) => {
  let error: string | null = null;

  if (typeof e === 'object' && Array.isArray((e as any)?.response?.errors)) {
    error = (e as any).response.errors.map((x: any) => x.message).join(', ');
  } else if (e instanceof Error) {
    error = e.message;
  } else if (e) {
    error = e.toString();
  }
  if (!error) {
    return null;
  }

  return (
    <div className="rounded-lg px-4 py-2 bg-red-500 text-white col-full">
      {errorTranslation[error] || (
        <>
          <div className="font-bold">
            {def || 'Něco se nepovedlo, zkuste to prosím znovu'}
          </div>
          <div className="text-sm">{error}</div>
        </>
      )}
    </div>
  );
};
