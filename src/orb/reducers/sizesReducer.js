export default (
  state = { rows: { leafs: {}, dimensions: {} }, columns: { leafs: {}, dimensions: {} } },
  action,
) => {
  const { type } = action;
  switch (type) {
    default:
      return state;
  }
};
