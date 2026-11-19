#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cmath>
#include "ucenv.h"

using namespace luna::ucenv;

#define GLOBAL_X0 -1.0
#define GLOBAL_Y0 -1.0
#define GLOBAL_D_X 2.0
#define GLOBAL_D_Y 2.0
#define ALPHA 1e5

static double Phi(double x, double y)
{
    return x * x + y * y + sin(M_PI*x*y) * sin(M_PI*x*y) + cos(M_PI*x*y) * cos(M_PI*x*y);
    // return x * x + y * y;
}

static double Rho(double x, double y)
{
    return 6 - ALPHA * Phi(x, y);
}

struct Grid
{
    int N_X;
    int N_Y;
    double X_0;
    double Y_0;
    double H_X;
    double H_Y;

    double *Data;

    int SerializedSize()
    {
        return sizeof(double) * N_X * N_Y + sizeof(int) * 2 + sizeof(double) * 4;
    }

    void *SerializeHeader(void *buf)
    {
        auto iBuf = (int *)buf;

        *iBuf = N_X;
        iBuf++;
        *iBuf = N_Y;
        iBuf++;

        auto dBuf = (double *)iBuf;
        *dBuf = X_0;
        dBuf++;
        *dBuf = Y_0;
        dBuf++;
        *dBuf = H_X;
        dBuf++;
        *dBuf = H_Y;
        dBuf++;

        return dBuf;
    }

    static Grid Attach(void const *buf)
    {
        auto grid = Grid{};

        auto iBuf = (int *)((void *)buf);

        grid.N_X = *iBuf;
        iBuf++;
        grid.N_Y = *iBuf;
        iBuf++;

        auto dBuf = (double *)iBuf;

        grid.X_0 = *dBuf;
        dBuf++;
        grid.Y_0 = *dBuf;
        dBuf++;

        grid.H_X = *dBuf;
        dBuf++;
        grid.H_Y = *dBuf;
        dBuf++;

        grid.Data = dBuf;

        return grid;
    }

    double *At(int i, int j)
    {
        assert(0 <= i && i < N_X);
        assert(0 <= j && j < N_Y);

        return &Data[j * N_X + i];
    }

    double X(int i)
    {
        assert(0 <= i && i < N_X);

        return X_0 + i * H_X;
    }

    double Y(int j)
    {
        assert(0 <= j && j < N_Y);

        return Y_0 + j * H_Y;
    }
};

static void SubgridSizeAndPositionNoBorders(int subgridIndex, int subgridsCount, int gridSize, int *subgridStartIndex, int *subgridSize)
{
    assert(NULL != subgridSize);
    assert(NULL != subgridStartIndex);
    assert(subgridIndex >= 0);
    assert(subgridsCount >= 0);
    assert(gridSize > 1);
    assert(gridSize >= subgridsCount && "Grid is too small");

    int const n = gridSize / subgridsCount;
    int const m = gridSize % subgridsCount;

    if (subgridIndex < m)
    {
        *subgridSize = n + 1;
        *subgridStartIndex = (n + 1) * subgridIndex;

        return;
    }

    *subgridSize = n;
    *subgridStartIndex = (n + 1) * m + n * (subgridIndex - m);
}

extern "C"
{
#pragma adapt user_type(subgrid_df, Subgrid)
    void init_subgrid(int n_x, int n_y, int subgrids_count_x, int subgrids_count_y, int subgrid_index_x, int subgrid_index_y, OutputDF &subgrid_df)
    {
        int subgridStartIndexX;
        int subgridSizeX;
        int subgridStartIndexY;
        int subgridSizeY;

        SubgridSizeAndPositionNoBorders(subgrid_index_x, subgrids_count_x, n_x - 2 /* exclude edges */, &subgridStartIndexX, &subgridSizeX);
        SubgridSizeAndPositionNoBorders(subgrid_index_y, subgrids_count_y, n_y - 2 /* exclude edges */, &subgridStartIndexY, &subgridSizeY);

        // Include edges
        subgridStartIndexX += 1;
        subgridStartIndexY += 1;

        // Include neighbor edges
        subgridStartIndexX -= 1;
        subgridStartIndexY -= 1;
        subgridSizeX += 2;
        subgridSizeY += 2;

        // printf("[%d, %d] x=%d..%d, y=%d..%d\n", subgridIndexX, subgridIndexY, subgridStartIndexX, subgridStartIndexX+subgridSizeX-1, subgridStartIndexY, subgridStartIndexY+subgridSizeY-1);

        double dX = GLOBAL_D_X * subgridSizeX / n_x;
        double dY = GLOBAL_D_Y * subgridSizeY / n_y;

        // printf("dX=%.2lf\n", dX);

        double hX = GLOBAL_D_X / (n_x - 1);
        double hY = GLOBAL_D_Y / (n_y - 1);

        // printf("hX=%.2lf\n", hX);

        double x0 = GLOBAL_X0 + subgridStartIndexX * hX;
        double y0 = GLOBAL_Y0 + subgridStartIndexY * hY;

        auto subgrid = Grid{
            /* .N_X = */ subgridSizeX,
            /* .N_Y = */ subgridSizeY,
            /* .X_0 = */ x0,
            /* .Y_0 = */ y0,
            /* .H_X = */ hX,
            /* .H_Y = */ hY};

        void *buf = subgrid_df.create(subgrid.SerializedSize());
        subgrid.Data = (double *)subgrid.SerializeHeader(buf);
        memset(subgrid.Data, 0, subgrid.N_X * subgrid.N_Y * sizeof(double));

        if (subgrid_index_x == 0)
        {
            int ii = 0;
            for (int jj = 0; jj < subgrid.N_Y; jj++)
            {
                *subgrid.At(ii, jj) = Phi(subgrid.X(ii), subgrid.Y(jj));
            }
        }

        if (subgrid_index_x == subgrids_count_x - 1)
        {
            int ii = subgrid.N_X - 1;
            for (int jj = 0; jj < subgrid.N_Y; jj++)
            {
                *subgrid.At(ii, jj) = Phi(subgrid.X(ii), subgrid.Y(jj));
            }
        }

        if (subgrid_index_y == 0)
        {
            int jj = 0;
            for (int ii = 0; ii < subgrid.N_X; ii++)
            {
                *subgrid.At(ii, jj) = Phi(subgrid.X(ii), subgrid.Y(jj));
            }
        }

        if (subgrid_index_y == subgrids_count_y - 1)
        {
            int jj = subgrid.N_Y - 1;
            for (int ii = 0; ii < subgrid.N_X; ii++)
            {
                *subgrid.At(ii, jj) = Phi(subgrid.X(ii), subgrid.Y(jj));
            }
        }
    }

#pragma adapt user_type(subgrid_df, Subgrid)
    void print_subgrid(const InputDF &subgrid_df)
    {
        auto subgrid = Grid::Attach(subgrid_df.get_data());

        printf("Subgrid{N_X=%d, N_Y=%d, X=%.2lf..%.2lf, Y=%.2lf..%.2lf, H_X=%.2lf, H_Y=%.2lf}\n", subgrid.N_X, subgrid.N_Y, subgrid.X_0, subgrid.X_0 + subgrid.H_X * (subgrid.N_X - 1), subgrid.Y_0, subgrid.Y_0 + subgrid.H_Y * (subgrid.N_Y - 1), subgrid.H_X, subgrid.H_Y);

        for (int j = 0; j < subgrid.N_Y; j++)
        {
            for (int i = 0; i < subgrid.N_X; i++)
            {
                printf("%.2lf ", *subgrid.At(i, j));
            }

            printf("\n");
        }
    }

#pragma adapt user_type(prev_df, Subgrid)
#pragma adapt user_type(next_no_bounds_df, Subgrid)
    void update_subgrid_no_edges(const InputDF &prev_df, OutputDF &next_no_bounds_df, OutputDF &delta)
    {
        auto prevSubgrid = Grid::Attach(prev_df.get_data());
        auto nextNoBounds = prevSubgrid;

        void *buf = next_no_bounds_df.create(nextNoBounds.SerializedSize());
        nextNoBounds.Data = (double *)nextNoBounds.SerializeHeader(buf);
        memcpy(nextNoBounds.Data, prevSubgrid.Data, sizeof(double) * nextNoBounds.N_X * nextNoBounds.N_Y);

        double hxSquared = prevSubgrid.H_X * prevSubgrid.H_X;
        double hySquared = prevSubgrid.H_Y * prevSubgrid.H_Y;
        double coeff = 1 / (2 / hxSquared + 2 / hySquared + ALPHA);

        double maxDelta = 0.0;

        for (int j = 1; j < prevSubgrid.N_Y - 1; j++)
        {
            for (int i = 1; i < prevSubgrid.N_X - 1; i++)
            {
                double rho = Rho(prevSubgrid.X(i), prevSubgrid.Y(j));

                double valueWest = *prevSubgrid.At(i - 1, j);
                double valueEast = *prevSubgrid.At(i + 1, j);
                double valueNorth = *prevSubgrid.At(i, j - 1);
                double valueSouth = *prevSubgrid.At(i, j + 1);

                *nextNoBounds.At(i, j) = coeff * ((valueWest + valueEast) / hxSquared + (valueNorth + valueSouth) / hySquared - rho);

                double curDelta = fabs(*prevSubgrid.At(i, j) - *nextNoBounds.At(i, j));
                if (curDelta > maxDelta)
                {
                    maxDelta = curDelta;
                }
            }
        }

        delta = maxDelta;
    }

#pragma adapt user_type(west_in, Edge)
#pragma adapt user_type(east_in, Edge)
#pragma adapt user_type(north_in, Edge)
#pragma adapt user_type(south_in, Edge)
    void print_edges(const InputDF &west_in, const InputDF &east_in, const InputDF &north_in, const InputDF &south_in)
    {
        auto west = west_in.getData<double>();
        auto east = east_in.getData<double>();
        auto north = north_in.getData<double>();
        auto south = south_in.getData<double>();

        if (west_in.getSize() > 0)
        {
            printf("west[%d]: ", (int) west_in.getSize());
            for (int i = 0; i < west_in.getSize() / sizeof(double); i++)
            {
                printf("%.2lf ", west[i]);
            }
            printf("\n");
        }

        if (east_in.getSize() > 0)
        {
            printf("east: ");
            for (int i = 0; i < east_in.getSize() / sizeof(double); i++)
            {
                printf("%.2lf ", east[i]);
            }
            printf("\n");
        }

        if (north_in.getSize() > 0)
        {
            printf("north[%d]: ", (int) north_in.getSize());
            for (int i = 0; i < north_in.getSize() / sizeof(double); i++)
            {
                printf("%.2lf ", north[i]);
            }
            printf("\n");
        }

        if (south_in.getSize() > 0)
        {
            printf("south: ");
            for (int i = 0; i < south_in.getSize() / sizeof(double); i++)
            {
                printf("%.2lf ", south[i]);
            }
            printf("\n");
        }

        printf("\n");
    }

#pragma adapt user_type(subgrid_no_edges_df, Subgrid)
#pragma adapt user_type(west_out, Edge)
#pragma adapt user_type(east_out, Edge)
#pragma adapt user_type(north_out, Edge)
#pragma adapt user_type(south_out, Edge)
    void extract_own_edges(const InputDF &subgrid_no_edges_df, OutputDF &west_out, OutputDF &east_out, OutputDF &north_out, OutputDF &south_out)
    {
        auto subgrid = Grid::Attach(subgrid_no_edges_df.get_data());

        auto west = west_out.create<double>(subgrid.N_Y - 2);
        auto east = east_out.create<double>(subgrid.N_Y - 2);
        auto north = north_out.create<double>(subgrid.N_X - 2);
        auto south = south_out.create<double>(subgrid.N_X - 2);

        {
            int ii = 1;
            for (int jj = 1; jj < subgrid.N_Y - 1; jj++)
            {
                west[jj - 1] = *subgrid.At(ii, jj);
            }
        }

        {
            int ii = subgrid.N_X - 2;
            for (int jj = 1; jj < subgrid.N_Y - 1; jj++)
            {
                east[jj - 1] = *subgrid.At(ii, jj);
            }
        }

        {
            int jj = 1;
            for (int ii = 1; ii < subgrid.N_X - 1; ii++)
            {
                north[ii - 1] = *subgrid.At(ii, jj);
            }
        }

        {
            int jj = subgrid.N_Y - 2;
            for (int ii = 1; ii < subgrid.N_X - 1; ii++)
            {
                south[ii - 1] = *subgrid.At(ii, jj);
            }
        }
    }

#pragma adapt user_type(subgrid_no_edges_df, Subgrid)
#pragma adapt user_type(west_in, Edge)
#pragma adapt user_type(east_in, Edge)
#pragma adapt user_type(north_in, Edge)
#pragma adapt user_type(south_in, Edge)
#pragma adapt user_type(subgrid_df, Subgrid)
    void set_neighbor_edges(
        const InputDF &subgrid_no_edges_df,
        const InputDF &west_in, const InputDF &east_in, const InputDF &north_in, const InputDF &south_in,
        OutputDF &subgrid_df)
    {
        auto subgridNoEdges = Grid::Attach(subgrid_no_edges_df.get_data());

        void *buf = subgrid_df.create(subgridNoEdges.SerializedSize());
        auto subgrid = subgridNoEdges;
        subgrid.Data = (double *)subgrid.SerializeHeader(buf);
        memcpy(subgrid.Data, subgridNoEdges.Data, sizeof(double) * subgrid.N_X * subgrid.N_Y);

        auto west = west_in.getData<double>();
        auto east = east_in.getData<double>();
        auto north = north_in.getData<double>();
        auto south = south_in.getData<double>();

        if (west_in.getSize() > 0)
        {
            int ii = 0;
            for (int jj = 1; jj < subgrid.N_Y - 1; jj++)
            {
                *subgrid.At(ii, jj) = west[jj - 1];
            }
        }

        if (east_in.getSize() > 0)
        {
            int ii = subgrid.N_X - 1;
            for (int jj = 1; jj < subgrid.N_Y - 1; jj++)
            {
                *subgrid.At(ii, jj) = east[jj - 1];
            }
        }

        if (north_in.getSize() > 0)
        {
            int jj = 0;
            for (int ii = 1; ii < subgrid.N_X - 1; ii++)
            {
                *subgrid.At(ii, jj) = north[ii - 1];
            }
        }

        if (south_in.getSize() > 0)
        {
            int jj = subgrid.N_Y - 1;
            for (int ii = 1; ii < subgrid.N_X - 1; ii++)
            {
                *subgrid.At(ii, jj) = south[ii - 1];
            }
        }
    }

#pragma adapt user_type(edge, Subgrid)
#pragma adapt user_type(copy, Edge)
    void copy_edge(const InputDF &edge, OutputDF &copy)
    {
        memcpy(copy.create(edge.getSize()), edge.get_data(), edge.get_size());
    }

#pragma adapt user_type(subgridDF, Subgrid)
    void check_subgrid(const InputDF &subgridDF, OutputDF &error)
    {
        auto subgrid = Grid::Attach(subgridDF.get_data());
        double maxDiff = 0;

        for (int j = 1; j < subgrid.N_Y - 1; j++)
        {
            for (int i = 1; i < subgrid.N_X - 1; i++)
            {
                double expected = Phi(subgrid.X(i), subgrid.Y(j));
                double actual = *subgrid.At(i, j);
                double absDiff = fabs(expected - actual);

                if (absDiff > maxDiff)
                {
                    maxDiff = absDiff;
                }
            }
        }

        error = maxDiff;
    }

#pragma adapt user_type(x, Edge)
    void set_empty(OutputDF &x)
    {
        x.create(0);
    }
}
