import type { PropsWithChildren } from 'react';
import { ScrollView, StyleSheet, View } from 'react-native';

interface ScreenProps extends PropsWithChildren {
  padded?: boolean;
}

export function Screen({ children, padded = true }: ScreenProps) {
  return (
    <View style={styles.root}>
      <ScrollView
        contentContainerStyle={padded ? styles.padded : undefined}
        keyboardShouldPersistTaps="handled"
      >
        {children}
      </ScrollView>
    </View>
  );
}

const styles = StyleSheet.create({
  root: {
    flex: 1,
    backgroundColor: '#f5f5f5',
  },
  padded: {
    padding: 20,
    paddingBottom: 40,
  },
});
