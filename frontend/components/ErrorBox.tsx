import { Alert, AlertTitle, Grid } from "@mui/material";

const errorTranslation: { [key: string]: string } = {
  ACCOUNT_NOT_FOUND: 'Zadaná kombinace jména a e-mailu neexistuje',
  INVALID_PASSWORD: 'Nesprávné heslo',
  ACCOUNT_DISABLED: 'Učet byl zablokován',
  ACCOUNT_NOT_CONFIRMED: 'Účet ještě nebyl potvrzen',
}

export const ErrorBox = ({ error: e, grid, default: def }: {
  error: any;
  grid?: boolean;
  default?: string;
}): JSX.Element | null => {
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

  const human = errorTranslation[error] || 'Něco se nepovedlo, zkuste to prosím znovu';
  const res = human || (def ? <><AlertTitle>{def}</AlertTitle>{error}</> : error);

  return grid ? (
    <Grid item xs={12}>
      <Alert severity="error">{res}</Alert>
    </Grid>
  ) : (
    <Alert severity="error">{res}</Alert>
  );
};