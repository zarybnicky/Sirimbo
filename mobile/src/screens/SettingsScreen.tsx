import React from 'react';
import { StyleSheet, Text, View } from 'react-native';
import { useAuth } from '../auth/AuthProvider';
import { Button } from '../components/Button';
import { Card } from '../components/Card';
import { Screen } from '../components/Screen';

export function SettingsScreen() {
  const { user, logout, refresh, status, error } = useAuth();
  const [refreshing, setRefreshing] = React.useState(false);
  const [loggingOut, setLoggingOut] = React.useState(false);

  const onRefresh = React.useCallback(async () => {
    try {
      setRefreshing(true);
      await refresh();
    } finally {
      setRefreshing(false);
    }
  }, [refresh]);

  const onLogout = React.useCallback(async () => {
    setLoggingOut(true);
    try {
      await logout();
    } finally {
      setLoggingOut(false);
    }
  }, [logout]);

  return (
    <Screen>
      <Card>
        <Text style={styles.heading}>Profil</Text>
        <View style={styles.row}>
          <Text style={styles.label}>Uživatel:</Text>
          <Text style={styles.value}>{user?.uLogin ?? user?.uEmail ?? '—'}</Text>
        </View>
        <View style={styles.row}>
          <Text style={styles.label}>Stav:</Text>
          <Text style={styles.value}>{translateStatus(status)}</Text>
        </View>
        {error ? <Text style={styles.error}>{error.message}</Text> : null}
        <Button onPress={onRefresh} loading={refreshing} variant="secondary">
          Obnovit profil
        </Button>
        <Button onPress={onLogout} loading={loggingOut}>
          Odhlásit se
        </Button>
      </Card>
    </Screen>
  );
}

function translateStatus(status: string) {
  switch (status) {
    case 'authenticated':
      return 'Přihlášen';
    case 'loading':
      return 'Načítám…';
    case 'initializing':
      return 'Spouštím aplikaci…';
    default:
      return 'Odhlášen';
  }
}

const styles = StyleSheet.create({
  heading: {
    fontSize: 22,
    fontWeight: '700',
  },
  row: {
    flexDirection: 'row',
    justifyContent: 'space-between',
    alignItems: 'center',
  },
  label: {
    color: '#4f4f4f',
    fontWeight: '500',
  },
  value: {
    fontWeight: '600',
  },
  error: {
    color: '#d32f2f',
    fontSize: 14,
  },
});
