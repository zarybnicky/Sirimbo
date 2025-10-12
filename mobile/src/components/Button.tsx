import type { PropsWithChildren } from 'react';
import { ActivityIndicator, Pressable, StyleSheet, Text } from 'react-native';

type ButtonProps = PropsWithChildren<{
  onPress?: () => void;
  disabled?: boolean;
  loading?: boolean;
  variant?: 'primary' | 'secondary';
}>;

export function Button({
  children,
  onPress,
  disabled,
  loading,
  variant = 'primary',
}: ButtonProps) {
  return (
    <Pressable
      accessibilityRole="button"
      style={({ pressed }) => [
        styles.base,
        variant === 'secondary' ? styles.secondary : styles.primary,
        pressed && styles.pressed,
        (disabled || loading) && styles.disabled,
      ]}
      onPress={disabled || loading ? undefined : onPress}
    >
      {loading ? <ActivityIndicator color="#ffffff" /> : (
        <Text style={variant === 'secondary' ? styles.secondaryLabel : styles.label}>
          {children}
        </Text>
      )}
    </Pressable>
  );
}

const styles = StyleSheet.create({
  base: {
    borderRadius: 12,
    paddingVertical: 14,
    paddingHorizontal: 16,
    alignItems: 'center',
    justifyContent: 'center',
  },
  primary: {
    backgroundColor: '#2f80ed',
  },
  secondary: {
    backgroundColor: '#e0e0e0',
  },
  pressed: {
    opacity: 0.9,
  },
  disabled: {
    opacity: 0.6,
  },
  label: {
    color: '#ffffff',
    fontWeight: '600',
    fontSize: 16,
  },
  secondaryLabel: {
    color: '#333333',
    fontWeight: '600',
    fontSize: 16,
  },
});
