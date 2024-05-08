#-------------------------------- Smooth moving average
def get_Window_vals(index1, index2, col, df):
    vals = df.loc[index1:index2, col].tolist()
    return vals

def _smooth_moving_avr_col(df,col,N):
    dfOriginal = df.copy()
    
    for index,row in df.iterrows():
        if(index<=N and index!=0): #first N, average
            _vals = get_Window_vals(0,index,col,dfOriginal)
            avr = sum(_vals)/len(_vals)
            df.loc[index,col] = avr
            
        if(index>N):
            _vals = get_Window_vals(index-N,index,col,dfOriginal)
            avr = sum(_vals)/len(_vals)
            df.loc[index,col] = avr
    return df
            
def smooth(df,cols,N):
    for col in cols:
        df = _smooth_moving_avr_col(df,col,N)
    return df


