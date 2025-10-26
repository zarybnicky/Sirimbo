declare module '*.png' {
  const value: string;
  export default value;
}

declare module '*.svg' {
  const value: React.ElementType;
  export default value;
}

declare module '*.jpg' {
  const value: string;
  export default value;
}

declare module '*.webp' {
  const value: string;
  export default value;
}

declare module '*.pdf' {
  const value: string;
  export default value;
}

export {};

declare global {
  interface Window {
    serwist?: import('@serwist/window').Serwist;
  }
}
