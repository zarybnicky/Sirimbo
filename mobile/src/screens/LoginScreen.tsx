import React from 'react';
import {
  KeyboardAvoidingView,
  Platform,
  StyleSheet,
  Text,
  TextInput,
  View,
} from 'react-native';
import { useAuth } from '../auth/AuthProvider';
import { Button } from '../components/Button';
import { Card } from '../components/Card';

export function LoginScreen() {
  const { login, error } = useAuth();
  const [loginValue, setLoginValue] = React.useState('');
  const [passwordValue, setPasswordValue] = React.useState('');
  const [submitting, setSubmitting] = React.useState(false);
  const [submitError, setSubmitError] = React.useState<string | null>(null);

  const onSubmit = React.useCallback(async () => {
    setSubmitError(null);
    setSubmitting(true);
    try {
      await login(loginValue.trim(), passwordValue);
    } catch (err) {
      setSubmitError((err as Error).message);
    } finally {
      setSubmitting(false);
    }
  }, [login, loginValue, passwordValue]);

  const disabled = submitting || !loginValue || !passwordValue;

  return (
    <KeyboardAvoidingView
      behavior={Platform.select({ ios: 'padding', android: undefined })}
      style={styles.container}
    >
      <Card>
        <Text style={styles.heading}>Přihlášení</Text>
        <View style={styles.fieldGroup}>
          <Text style={styles.label}>E-mail nebo přihlašovací jméno</Text>
          <TextInput
            value={loginValue}
            onChangeText={setLoginValue}
            autoCapitalize="none"
            autoComplete="email"
            keyboardType="email-address"
            style={styles.input}
            placeholder="jana.novakova@example.cz"
            textContentType="username"
          />
        </View>
        <View style={styles.fieldGroup}>
          <Text style={styles.label}>Heslo</Text>
          <TextInput
            value={passwordValue}
            onChangeText={setPasswordValue}
            secureTextEntry
            style={styles.input}
            placeholder="••••••••"
            textContentType="password"
          />
        </View>
        {submitError ? <Text style={styles.error}>{submitError}</Text> : null}
        {!submitError && error ? <Text style={styles.error}>{error.message}</Text> : null}
        <Button onPress={onSubmit} loading={submitting} disabled={disabled}>
          Přihlásit se
        </Button>
      </Card>
    </KeyboardAvoidingView>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    alignItems: 'center',
    justifyContent: 'center',
    padding: 24,
    backgroundColor: '#f5f5f5',
  },
  heading: {
    fontSize: 24,
    fontWeight: '700',
    marginBottom: 12,
  },
  fieldGroup: {
    width: '100%',
    marginBottom: 12,
  },
  label: {
    fontSize: 14,
    color: '#4f4f4f',
    fontWeight: '500',
  },
  input: {
    borderRadius: 10,
    borderWidth: 1,
    borderColor: '#e0e0e0',
    paddingHorizontal: 14,
    paddingVertical: 12,
    fontSize: 16,
    backgroundColor: '#ffffff',
  },
  error: {
    color: '#d32f2f',
    marginTop: 8,
  },
});
